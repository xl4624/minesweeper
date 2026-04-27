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

let client_ip req =
  let headers = S.Request.headers req in
  match S.Headers.get "x-forwarded-for" headers with
  | Some xff ->
    (* X-Forwarded-For: client, proxy1, proxy2 - first entry is the client. *)
    (match String.split_on_char ',' xff with
     | first :: _ -> String.trim first
     | [] -> "?")
  | None ->
    (match S.Request.client_addr req with
     | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
     | Unix.ADDR_UNIX s -> "unix:" ^ s)
;;

module Game = struct
  let rows = 9
  let cols = 9
  let n_mines = 10
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

  let persist_version = 2

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

  let maybe_restart t =
    match t.status with
    | Active -> ()
    | Won | Lost ->
      if now () -. t.finished_at > restart_cooldown_s
      then (
        t.grid <- fresh_grid ();
        t.status <- Active;
        t.started_at <- now ();
        t.finished_at <- 0.0;
        save_locked t)
  ;;

  (* Caller holds the mutex. *)
  let click_locked t r c =
    maybe_restart t;
    if t.status <> Active
    then ()
    else if not (in_bounds r c)
    then ()
    else (
      let cell = t.grid.(r).(c) in
      if cell.revealed || cell.flagged
      then ()
      else if cell.mine
      then (
        cell.revealed <- true;
        t.status <- Lost;
        t.finished_at <- now ();
        t.losses <- t.losses + 1;
        save_locked t)
      else (
        flood_fill t.grid r c;
        if all_safe_revealed t.grid
        then (
          t.status <- Won;
          t.finished_at <- now ();
          t.wins <- t.wins + 1);
        save_locked t))
  ;;

  (* Caller holds the mutex. Toggle flag on a covered cell. *)
  let flag_locked t r c =
    maybe_restart t;
    if t.status <> Active
    then ()
    else if not (in_bounds r c)
    then ()
    else (
      let cell = t.grid.(r).(c) in
      if cell.revealed
      then ()
      else (
        cell.flagged <- not cell.flagged;
        save_locked t))
  ;;

  let with_lock t f =
    Mutex.lock t.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f
  ;;

  let click t r c = with_lock t (fun () -> click_locked t r c)
  let flag t r c = with_lock t (fun () -> flag_locked t r c)

  let sprite_for t r c =
    let cell = t.grid.(r).(c) in
    let n () = string_of_int (count_adjacent_mines t.grid r c) in
    match t.status with
    | Active -> if cell.revealed then n () else if cell.flagged then "flag" else "covered"
    | Lost ->
      if cell.mine && cell.revealed
      then "mine_exploded"
      else if cell.flagged
      then "flag"
      else if cell.mine
      then "mine"
      else if cell.revealed
      then n ()
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

let click_handler game limiter r c req =
  if Rate_limit.allow limiter (client_ip req) then Game.click game r c;
  redirect_response profile_url
;;

let flag_handler game limiter r c req =
  if Rate_limit.allow limiter (client_ip req) then Game.flag game r c;
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
  let server = S.create ~port () in
  S.add_route_handler ~meth:`GET server S.Route.(return) (fun req -> index_handler req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "click" @/ int @/ int @/ return)
    (fun r c req -> click_handler game limiter r c req);
  S.add_route_handler
    ~meth:`GET
    server
    S.Route.(exact "flag" @/ int @/ int @/ return)
    (fun r c req -> flag_handler game limiter r c req);
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
