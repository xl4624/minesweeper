module S = Tiny_httpd

let getenv_required name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
    Printf.eprintf "error: %s must be set\n" name;
    exit 2
;;

let profile_url = getenv_required "PROFILE_URL"

let assets_dir =
  match Sys.getenv_opt "ASSETS_DIR" with
  | Some d -> d
  | None -> "assets"
;;

let state_file =
  match Sys.getenv_opt "STATE_FILE" with
  | Some f -> f
  | None -> "state.bin"
;;

let port =
  match Sys.getenv_opt "PORT" with
  | Some s -> int_of_string s
  | None -> 8080
;;

let min_click_interval_s =
  match Sys.getenv_opt "MIN_CLICK_INTERVAL_S" with
  | Some s -> float_of_string s
  | None -> 1.0
;;

let log_file =
  match Sys.getenv_opt "LOG_FILE" with
  | Some f -> f
  | None -> "mines.log"
;;

let log_max_bytes =
  match Sys.getenv_opt "LOG_MAX_BYTES" with
  | Some s -> int_of_string s
  | None -> 1_000_000
;;

module Rate_limit = struct
  type t =
    { last_click : (string, float) Hashtbl.t
    ; mutex : Mutex.t
    ; min_interval : float
    }

  let create ~min_interval =
    { last_click = Hashtbl.create 256; mutex = Mutex.create (); min_interval }
  ;;

  (* Returns true if the action is allowed. Updates last-click time on allow. *)
  let allow t ip =
    Mutex.lock t.mutex;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock t.mutex)
      (fun () ->
         let now = Unix.gettimeofday () in
         match Hashtbl.find_opt t.last_click ip with
         | Some last when now -. last < t.min_interval -> false
         | _ ->
           Hashtbl.replace t.last_click ip now;
           true)
  ;;
end

(* Append-only log with size cap. When the active file passes [max_bytes],
   it is renamed to [<path>.old] (overwriting any prior .old) and a fresh
   file is opened. Total on-disk usage is therefore bounded by ~2*max_bytes. *)
module Logger = struct
  type t =
    { path : string
    ; max_bytes : int
    ; mutable oc : out_channel
    ; mutable bytes : int
    ; mutex : Mutex.t
    }

  let open_append path =
    let oc = open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o644 path in
    let bytes = try (Unix.stat path).st_size with _ -> 0 in
    oc, bytes
  ;;

  let create ~path ~max_bytes =
    let oc, bytes = open_append path in
    { path; max_bytes; oc; bytes; mutex = Mutex.create () }
  ;;

  let rotate_locked t =
    close_out_noerr t.oc;
    (try Sys.rename t.path (t.path ^ ".old") with _ -> ());
    let oc, bytes = open_append t.path in
    t.oc <- oc;
    t.bytes <- bytes
  ;;

  let log t fmt =
    Printf.ksprintf
      (fun msg ->
         let tm = Unix.localtime (Unix.gettimeofday ()) in
         let line =
           Printf.sprintf
             "%04d-%02d-%02d %02d:%02d:%02d %s\n"
             (tm.Unix.tm_year + 1900)
             (tm.tm_mon + 1)
             tm.tm_mday
             tm.tm_hour
             tm.tm_min
             tm.tm_sec
             msg
         in
         let len = String.length line in
         Mutex.lock t.mutex;
         Fun.protect
           ~finally:(fun () -> Mutex.unlock t.mutex)
           (fun () ->
              if t.bytes + len > t.max_bytes then rotate_locked t;
              (try
                 output_string t.oc line;
                 flush t.oc;
                 t.bytes <- t.bytes + len
               with _ -> ()));
         prerr_string line)
      fmt
  ;;
end

let client_ip req =
  let headers = S.Request.headers req in
  match S.Headers.get "x-forwarded-for" headers with
  | Some xff ->
    (* Caddy appends the real peer to the right end of XFF, so the rightmost
       entry is the IP it observed. The leftmost is whatever the client sent
       and is spoofable. *)
    (match List.rev (String.split_on_char ',' xff) with
     | last :: _ -> String.trim last
     | [] -> "?")
  | None ->
    (match S.Request.client_addr req with
     | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
     | Unix.ADDR_UNIX s -> "unix:" ^ s)
;;

module Game = struct
  let rows = 6
  let cols = 6
  let n_mines = 5
  let restart_cooldown_s = 0.0

  type cell =
    { mutable mine : bool
    ; mutable revealed : bool
    ; mutable flagged : bool
    }

  type status =
    | Active
    | Won
    | Lost

  type t =
    { mutable grid : cell array array
    ; mutable status : status
    ; mutable started_at : float
    ; mutable finished_at : float
    ; mutable wins : int
    ; mutable losses : int
    ; state_file : string
    ; mutex : Mutex.t
    }

  (* On-disk shape. Same fields as [t] minus the mutex; bumped if the
     layout ever changes incompatibly. *)
  type persisted =
    { p_version : int
    ; p_grid : cell array array
    ; p_status : status
    ; p_started_at : float
    ; p_finished_at : float
    ; p_wins : int
    ; p_losses : int
    }

  let persist_version = 3

  let to_persisted t =
    { p_version = persist_version
    ; p_grid = t.grid
    ; p_status = t.status
    ; p_started_at = t.started_at
    ; p_finished_at = t.finished_at
    ; p_wins = t.wins
    ; p_losses = t.losses
    }
  ;;

  let of_persisted state_file p =
    (* Remove any ghost flags on revealed cells. *)
    Array.iter
      (fun row ->
         Array.iter (fun c -> if c.revealed && c.flagged then c.flagged <- false) row)
      p.p_grid;
    { grid = p.p_grid
    ; status = p.p_status
    ; started_at = p.p_started_at
    ; finished_at = p.p_finished_at
    ; wins = p.p_wins
    ; losses = p.p_losses
    ; state_file
    ; mutex = Mutex.create ()
    }
  ;;

  let now () = Unix.gettimeofday ()

  let fresh_grid () =
    let g =
      Array.init rows (fun _ ->
        Array.init cols (fun _ -> { mine = false; revealed = false; flagged = false }))
    in
    let placed = ref 0 in
    while !placed < n_mines do
      let r = Random.int rows
      and c = Random.int cols in
      if not g.(r).(c).mine
      then (
        g.(r).(c).mine <- true;
        incr placed)
    done;
    g
  ;;

  let fresh state_file =
    { grid = fresh_grid ()
    ; status = Active
    ; started_at = now ()
    ; finished_at = 0.0
    ; wins = 0
    ; losses = 0
    ; state_file
    ; mutex = Mutex.create ()
    }
  ;;

  (* Caller holds the mutex. Atomic-on-rename. *)
  let save_locked t =
    let tmp = t.state_file ^ ".tmp" in
    let oc = open_out_bin tmp in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> Marshal.to_channel oc (to_persisted t) []);
    Sys.rename tmp t.state_file
  ;;

  let load_or_fresh state_file =
    if not (Sys.file_exists state_file)
    then fresh state_file
    else (
      try
        let ic = open_in_bin state_file in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
             let p : persisted = Marshal.from_channel ic in
             if p.p_version <> persist_version
             then (
               Printf.eprintf
                 "state file version %d != expected %d, starting fresh\n%!"
                 p.p_version
                 persist_version;
               fresh state_file)
             else of_persisted state_file p)
      with
      | e ->
        Printf.eprintf
          "failed to load state from %s: %s; starting fresh\n%!"
          state_file
          (Printexc.to_string e);
        fresh state_file)
  ;;

  let in_bounds r c = r >= 0 && r < rows && c >= 0 && c < cols

  let count_adjacent_mines g r c =
    let n = ref 0 in
    for dr = -1 to 1 do
      for dc = -1 to 1 do
        if not (dr = 0 && dc = 0)
        then (
          let nr = r + dr
          and nc = c + dc in
          if in_bounds nr nc && g.(nr).(nc).mine then incr n)
      done
    done;
    !n
  ;;

  let flood_fill g r c =
    let q = Queue.create () in
    Queue.add (r, c) q;
    while not (Queue.is_empty q) do
      let r, c = Queue.pop q in
      if in_bounds r c && (not g.(r).(c).revealed) && not g.(r).(c).mine
      then (
        g.(r).(c).revealed <- true;
        g.(r).(c).flagged <- false;
        if count_adjacent_mines g r c = 0
        then
          for dr = -1 to 1 do
            for dc = -1 to 1 do
              if not (dr = 0 && dc = 0) then Queue.add (r + dr, c + dc) q
            done
          done)
    done
  ;;

  let all_safe_revealed g =
    let result = ref true in
    Array.iter
      (fun row ->
         Array.iter
           (fun cell -> if (not cell.mine) && not cell.revealed then result := false)
           row)
      g;
    !result
  ;;

  let any_revealed g =
    let any = ref false in
    Array.iter (fun row -> Array.iter (fun c -> if c.revealed then any := true) row) g;
    !any
  ;;

  (* Move the mine at (r,c) to the first non-mine cell that isn't (r,c). *)
  let relocate_mine g r c =
    let moved = ref false in
    let r' = ref 0 in
    while (not !moved) && !r' < rows do
      let c' = ref 0 in
      while (not !moved) && !c' < cols do
        if (!r' <> r || !c' <> c) && not g.(!r').(!c').mine
        then (
          g.(!r').(!c').mine <- true;
          g.(r).(c).mine <- false;
          moved := true);
        incr c'
      done;
      incr r'
    done
  ;;

  type click_outcome =
    | Click_hit_mine
    | Click_won
    | Click_revealed
    | Click_noop_revealed
    | Click_noop_flagged
    | Click_noop_inactive
    | Click_noop_bounds

  type flag_outcome =
    | Flag_on
    | Flag_off
    | Flag_noop_revealed
    | Flag_noop_cap
    | Flag_noop_inactive
    | Flag_noop_bounds

  let string_of_click_outcome = function
    | Click_hit_mine -> "hit_mine"
    | Click_won -> "won"
    | Click_revealed -> "revealed"
    | Click_noop_revealed -> "noop_revealed"
    | Click_noop_flagged -> "noop_flagged"
    | Click_noop_inactive -> "noop_inactive"
    | Click_noop_bounds -> "noop_bounds"
  ;;

  let string_of_flag_outcome = function
    | Flag_on -> "flag_on"
    | Flag_off -> "flag_off"
    | Flag_noop_revealed -> "noop_revealed"
    | Flag_noop_cap -> "noop_cap"
    | Flag_noop_inactive -> "noop_inactive"
    | Flag_noop_bounds -> "noop_bounds"
  ;;

  (* Caller holds the mutex. Returns whether a restart was performed. *)
  let maybe_restart_locked t =
    match t.status with
    | Active -> false
    | Won | Lost ->
      if now () -. t.finished_at > restart_cooldown_s
      then (
        t.grid <- fresh_grid ();
        t.status <- Active;
        t.started_at <- now ();
        t.finished_at <- 0.0;
        save_locked t;
        true)
      else false
  ;;

  (* Caller holds the mutex. *)
  let click_locked t r c =
    if t.status <> Active
    then Click_noop_inactive
    else if not (in_bounds r c)
    then Click_noop_bounds
    else (
      let cell = t.grid.(r).(c) in
      if cell.revealed
      then Click_noop_revealed
      else if cell.flagged
      then Click_noop_flagged
      else (
        (* First-click safe: if no cell has been revealed yet, never let
           the player lose on move 1. Move the mine elsewhere. *)
        if cell.mine && not (any_revealed t.grid) then relocate_mine t.grid r c;
        if cell.mine
        then (
          cell.revealed <- true;
          t.status <- Lost;
          t.finished_at <- now ();
          t.losses <- t.losses + 1;
          save_locked t;
          Click_hit_mine)
      else (
        flood_fill t.grid r c;
        let won = all_safe_revealed t.grid in
        if won
        then (
          t.status <- Won;
          t.finished_at <- now ();
          t.wins <- t.wins + 1);
        save_locked t;
        if won then Click_won else Click_revealed)))
  ;;

  let count_flags g =
    let n = ref 0 in
    Array.iter (fun row -> Array.iter (fun c -> if c.flagged then incr n) row) g;
    !n
  ;;

  (* Caller holds the mutex. Toggle flag on a covered cell. *)
  let flag_locked t r c =
    if t.status <> Active
    then Flag_noop_inactive
    else if not (in_bounds r c)
    then Flag_noop_bounds
    else (
      let cell = t.grid.(r).(c) in
      if cell.revealed
      then Flag_noop_revealed
      else if (not cell.flagged) && count_flags t.grid >= n_mines
      then Flag_noop_cap
      else (
        cell.flagged <- not cell.flagged;
        save_locked t;
        if cell.flagged then Flag_on else Flag_off))
  ;;

  let with_lock t f =
    Mutex.lock t.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f
  ;;

  let click t r c =
    with_lock t (fun () ->
      let restarted = maybe_restart_locked t in
      let outcome = click_locked t r c in
      restarted, outcome)
  ;;

  let flag t r c =
    with_lock t (fun () ->
      let restarted = maybe_restart_locked t in
      let outcome = flag_locked t r c in
      restarted, outcome)
  ;;

  let sprite_for t r c =
    let cell = t.grid.(r).(c) in
    let n () = string_of_int (count_adjacent_mines t.grid r c) in
    match t.status with
    | Active -> if cell.revealed then n () else if cell.flagged then "flag" else "covered"
    | Lost ->
      if cell.revealed && cell.mine
      then "mine_exploded"
      else if cell.revealed
      then n ()
      else if cell.flagged
      then "flag"
      else if cell.mine
      then "mine"
      else "covered"
    | Won -> if cell.mine then "flag" else n ()
  ;;
end

let sprites : (string, string) Hashtbl.t = Hashtbl.create 16

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf
;;

let load_sprites () =
  let names =
    [ "covered"
    ; "0"
    ; "1"
    ; "2"
    ; "3"
    ; "4"
    ; "5"
    ; "6"
    ; "7"
    ; "8"
    ; "mine"
    ; "mine_exploded"
    ; "flag"
    ; "flag_button"
    ]
  in
  List.iter
    (fun name ->
       let path = Filename.concat assets_dir (name ^ ".png") in
       Hashtbl.replace sprites name (read_file path))
    names
;;

let no_cache =
  [ "Cache-Control", "no-cache, no-store, must-revalidate"
  ; "Pragma", "no-cache"
  ; "Expires", "0"
  ]
;;

let png_response body =
  let headers = ("Content-Type", "image/png") :: no_cache in
  S.Response.make_raw ~headers ~code:200 body
;;

let svg_response body =
  let headers = ("Content-Type", "image/svg+xml; charset=utf-8") :: no_cache in
  S.Response.make_raw ~headers ~code:200 body
;;

let redirect_response url =
  let headers = ("Location", url) :: no_cache in
  S.Response.make_raw ~headers ~code:302 ""
;;

(* Approximates shields.io flat style. Char width is approximated at 7px
   for Verdana 11; fine for short ASCII labels and integer values. *)
let badge_svg ~label ~value ~color =
  let char_w = 7 in
  let pad = 6 in
  let label_w = (String.length label * char_w) + (2 * pad) in
  let value_w = (String.length value * char_w) + (2 * pad) in
  let total_w = label_w + value_w in
  let label_cx = label_w / 2 in
  let value_cx = label_w + (value_w / 2) in
  Printf.sprintf
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" \
     height=\"20\"><linearGradient id=\"g\" x2=\"0\" y2=\"100%%\"><stop offset=\"0\" \
     stop-color=\"#bbb\" stop-opacity=\".1\"/><stop offset=\"1\" \
     stop-opacity=\".1\"/></linearGradient><clipPath id=\"c\"><rect width=\"%d\" \
     height=\"20\" rx=\"3\" fill=\"#fff\"/></clipPath><g clip-path=\"url(#c)\"><rect \
     width=\"%d\" height=\"20\" fill=\"#555\"/><rect x=\"%d\" width=\"%d\" height=\"20\" \
     fill=\"%s\"/><rect width=\"%d\" height=\"20\" fill=\"url(#g)\"/></g><g \
     fill=\"#fff\" text-anchor=\"middle\" font-family=\"Verdana,Geneva,sans-serif\" \
     font-size=\"11\"><text x=\"%d\" y=\"15\" fill=\"#010101\" \
     fill-opacity=\".3\">%s</text><text x=\"%d\" y=\"14\">%s</text><text x=\"%d\" \
     y=\"15\" fill=\"#010101\" fill-opacity=\".3\">%s</text><text x=\"%d\" \
     y=\"14\">%s</text></g></svg>"
    total_w
    total_w
    label_w
    label_w
    value_w
    color
    total_w
    label_cx
    label
    label_cx
    label
    value_cx
    value
    value_cx
    value
;;

let cell_handler game r c _req =
  if not (Game.in_bounds r c)
  then S.Response.make_string ~code:404 (Ok "out of range")
  else (
    let sprite = Game.with_lock game (fun () -> Game.sprite_for game r c) in
    let body = Hashtbl.find sprites sprite in
    png_response body)
;;

let click_handler game limiter logger r c req =
  let ip = client_ip req in
  if Rate_limit.allow limiter ip
  then (
    let restarted, outcome = Game.click game r c in
    Logger.log
      logger
      "click ip=%s r=%d c=%d allowed restarted=%b outcome=%s"
      ip
      r
      c
      restarted
      (Game.string_of_click_outcome outcome))
  else Logger.log logger "click ip=%s r=%d c=%d denied=ratelimit" ip r c;
  redirect_response profile_url
;;

let flag_handler game limiter logger r c req =
  let ip = client_ip req in
  if Rate_limit.allow limiter ip
  then (
    let restarted, outcome = Game.flag game r c in
    Logger.log
      logger
      "flag ip=%s r=%d c=%d allowed restarted=%b outcome=%s"
      ip
      r
      c
      restarted
      (Game.string_of_flag_outcome outcome))
  else Logger.log logger "flag ip=%s r=%d c=%d denied=ratelimit" ip r c;
  redirect_response profile_url
;;

let render_index () =
  let buf = Buffer.create 4096 in
  Buffer.add_string
    buf
    "<!DOCTYPE html>\n\
     <html><head><title>minesweeper</title>\n\
     <style>*{margin:0;padding:0;box-sizing:border-box}body{background:#222;color:#ddd;font-family:monospace;display:flex;flex-direction:column;align-items:center;padding:24px;gap:12px}table{border-collapse:collapse;border-spacing:0;border:0;line-height:0;font-size:0}tr,td{padding:0;border:0;line-height:0;font-size:0;vertical-align:top}a{display:block;line-height:0;text-decoration:none}img{display:block;border:0;vertical-align:top;image-rendering:pixelated;image-rendering:crisp-edges}</style>\n\
     </head><body>\n\
     <h2>minesweeper</h2>\n";
  Buffer.add_string buf "<table cellspacing=\"0\" cellpadding=\"0\" border=\"0\">\n";
  for r = 0 to Game.rows - 1 do
    Buffer.add_string buf "  <tr>";
    for c = 0 to Game.cols - 1 do
      Printf.bprintf
        buf
        "<td><a href=\"/click/%d/%d\"><img src=\"/cell/%d/%d\" width=\"32\" \
         height=\"32\" alt=\"\"/></a><a href=\"/flag/%d/%d\"><img \
         src=\"/flag_button.png\" width=\"32\" height=\"14\" alt=\"flag\"/></a></td>"
        r
        c
        r
        c
        r
        c
    done;
    Buffer.add_string buf "</tr>\n"
  done;
  Buffer.add_string buf "</table>\n<p><a href=\"/stats\">stats</a></p>\n";
  Buffer.add_string buf "</body></html>\n";
  Buffer.contents buf
;;

let index_handler _req =
  let headers = ("Content-Type", "text/html; charset=utf-8") :: no_cache in
  S.Response.make_raw ~headers ~code:200 (render_index ())
;;

let status_string = function
  | Game.Active -> "active"
  | Game.Won -> "won"
  | Game.Lost -> "lost"
;;

let stats_handler game _req =
  let s =
    Game.with_lock game (fun () ->
      let { Game.wins; losses; status; _ } = game in
      Printf.sprintf "wins=%d losses=%d status=%s\n" wins losses (status_string status))
  in
  S.Response.make_string (Ok s)
;;

let wins_badge_handler game _req =
  let value = Game.with_lock game (fun () -> string_of_int game.Game.wins) in
  svg_response (badge_svg ~label:"wins" ~value ~color:"#4c1")
;;

let losses_badge_handler game _req =
  let value = Game.with_lock game (fun () -> string_of_int game.Game.losses) in
  svg_response (badge_svg ~label:"losses" ~value ~color:"#e05d44")
;;

let stats_json_handler game _req =
  let body =
    Game.with_lock game (fun () ->
      let { Game.wins; losses; status; _ } = game in
      Printf.sprintf
        "{\"wins\":%d,\"losses\":%d,\"status\":\"%s\"}\n"
        wins
        losses
        (status_string status))
  in
  let headers = ("Content-Type", "application/json") :: no_cache in
  S.Response.make_raw ~headers ~code:200 body
;;

let () =
  Random.self_init ();
  load_sprites ();
  let game = Game.load_or_fresh state_file in
  let limiter = Rate_limit.create ~min_interval:min_click_interval_s in
  let logger = Logger.create ~path:log_file ~max_bytes:log_max_bytes in
  Logger.log
    logger
    "startup port=%d state=%s log=%s log_max_bytes=%d min_click_interval_s=%g"
    port
    state_file
    log_file
    log_max_bytes
    min_click_interval_s;
  let server = S.create ~port () in
  S.add_route_handler ~meth:`GET server S.Route.(return) (fun req -> index_handler req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "click" @/ int @/ int @/ return)
    (fun r c req -> click_handler game limiter logger r c req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "flag" @/ int @/ int @/ return)
    (fun r c req -> flag_handler game limiter logger r c req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "cell" @/ int @/ int @/ return)
    (fun r c req -> cell_handler game r c req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "flag_button.png" @/ return)
    (fun _req -> png_response (Hashtbl.find sprites "flag_button"));
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "stats" @/ return)
    (fun req -> stats_handler game req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "stats.json" @/ return)
    (fun req -> stats_json_handler game req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "wins.svg" @/ return)
    (fun req -> wins_badge_handler game req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "losses.svg" @/ return)
    (fun req -> losses_badge_handler game req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "healthz" @/ return)
    (fun _req -> S.Response.make_string (Ok "ok"));
  Printf.printf
    "minesweeper listening on http://%s:%d (profile=%s, state=%s)\n%!"
    (S.addr server)
    (S.port server)
    profile_url
    state_file;
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
;;
