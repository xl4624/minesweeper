# minesweeper

Multiplayer minesweeper that lives in a GitHub profile README.

One global game shared across all visitors. 9x9 board, 10 mines. Each cell has
a reveal link (the cell image) and a flag toggle below it. After a win or loss
the game auto-resets on the next request that arrives more than 30s after
game-over. Per-IP rate limit on click/flag (default 1 click/second/IP).

## Build and run locally

```bash
dune build
dune exec bin/main.exe
```

`PROFILE_URL` is required (the URL the browser is redirected back to after a
click). For local testing, set it to `http://localhost:8080/` to bounce back
to the built-in index page. The other env vars are optional:

- `PORT` (default `8080`)
- `ASSETS_DIR` (default `assets`)
- `STATE_FILE` (default `state.bin`) - path to the persisted game state.
  After every mutation the state is Marshal-written to a `.tmp` file and
  atomically renamed, so a server restart resumes the same game. Mount a
  volume here when deploying.
- `MIN_CLICK_INTERVAL_S` (default `1.0`) - per-IP minimum seconds between
  click or flag requests. Blocked clicks silently redirect without applying.
  Behind a reverse proxy, the IP comes from `X-Forwarded-For`.

## Endpoints

- `GET /cell/:r/:c` - PNG sprite for the current state of cell `(r,c)`. Sent
  with `Cache-Control: no-cache` so GitHub's image proxy re-fetches every load.
- `GET /click/:r/:c` - apply a reveal at `(r,c)`, then `302` to the profile.
  No-op if the cell is already revealed or flagged.
- `GET /flag/:r/:c` - toggle a flag at `(r,c)`, then `302` to the profile.
  No-op if the cell is already revealed.
- `GET /flag_button.png` - sprite used for the flag toggle button under each
  cell. Identical for every cell, so it caches well.
- `GET /stats` - plaintext wins/losses/status.
- `GET /stats.json` - same data as JSON.
- `GET /wins.svg`, `GET /losses.svg` - shields.io-style flat badges,
  served with `no-cache` so they update at the same speed as the board.
  Use these instead of pointing shields.io at `/stats.json`, since
  shields.io enforces a multi-minute minimum cache on dynamic badges.
- `GET /healthz` - liveness probe.

## Regenerating assets

```bash
python3 scripts/gen_sprites.py    # rewrites assets/*.png
```

## Generating the profile README block

```bash
python3 scripts/gen_readme.py --server https://your.server.url
```

