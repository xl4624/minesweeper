#!/usr/bin/env bash
# Bootstrap the minesweeper server on a fresh Ubuntu 24.04 host.
#
# Required env:
#   DOMAIN
#   REPO_URL
#   PROFILE_URL
# Optional env:
#   OCAML_VERSION=5.4.1                                 (default)
#   PORT=8080                                           (default)
#   MIN_CLICK_INTERVAL_S=1.0                            (default)

set -euo pipefail

DOMAIN="${DOMAIN:?Set DOMAIN env}"
REPO_URL="${REPO_URL:?Set REPO_URL env}"
PROFILE_URL="${PROFILE_URL:?Set PROFILE_URL env}"
OCAML_VERSION="${OCAML_VERSION:-5.4.1}"
PORT="${PORT:-8080}"
MIN_CLICK_INTERVAL_S="${MIN_CLICK_INTERVAL_S:-1.0}"

APP_DIR=/opt/minesweeper
STATE_DIR=/var/lib/minesweeper
SERVICE_USER=minesweeper
BUILD_USER=ubuntu

if [ "$EUID" -ne 0 ]; then
  echo "must run as root (try: sudo bash $0)" >&2
  exit 1
fi

echo "[1/8] swap file (build is tight on 1GB RAM)"
if ! swapon --show | grep -q /swapfile; then
  fallocate -l 2G /swapfile
  chmod 600 /swapfile
  mkswap /swapfile
  swapon /swapfile
  grep -q '^/swapfile' /etc/fstab || echo "/swapfile none swap sw 0 0" >> /etc/fstab
fi

echo "[2/8] apt deps"
apt-get update -y
DEBIAN_FRONTEND=noninteractive apt-get install -y \
  build-essential m4 unzip pkg-config bubblewrap git curl gnupg \
  opam debian-keyring debian-archive-keyring apt-transport-https

echo "[2.5/8] clone or sync repo"
if [ -d "$APP_DIR/.git" ]; then
  sudo -u "$BUILD_USER" git -C "$APP_DIR" fetch --prune origin
  sudo -u "$BUILD_USER" git -C "$APP_DIR" reset --hard origin/main
else
  rm -rf "$APP_DIR"
  install -d -o "$BUILD_USER" -g "$BUILD_USER" "$APP_DIR"
  sudo -u "$BUILD_USER" git clone "$REPO_URL" "$APP_DIR"
fi

echo "[3/8] caddy repo + install"
if ! command -v caddy >/dev/null; then
  curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' \
    | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
  curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' \
    | tee /etc/apt/sources.list.d/caddy-stable.list >/dev/null
  apt-get update -y
  apt-get install -y caddy
fi

echo "[4/8] opam init + switch (as $BUILD_USER)"
sudo -u "$BUILD_USER" -H bash -s <<EOF
set -euo pipefail
if [ ! -d ~/.opam ]; then
  opam init --bare --disable-sandboxing -y
fi
eval \$(opam env --set-switch 2>/dev/null) || true
if ! opam switch list -s | grep -qx "$OCAML_VERSION"; then
  opam switch create "$OCAML_VERSION" -y -j 1
fi
eval \$(opam env --switch="$OCAML_VERSION" --set-switch)
opam install -y tiny_httpd dune
EOF

echo "[5/8] build"
chown -R "$BUILD_USER:$BUILD_USER" "$APP_DIR"
sudo -u "$BUILD_USER" -H bash -c "
  set -euo pipefail
  eval \$(opam env --switch='$OCAML_VERSION' --set-switch)
  cd '$APP_DIR'
  dune build --profile release
"

echo "[6/8] system user + state dir"
id -u "$SERVICE_USER" >/dev/null 2>&1 || \
  useradd --system --home "$APP_DIR" --shell /usr/sbin/nologin "$SERVICE_USER"
install -d -o "$SERVICE_USER" -g "$SERVICE_USER" "$STATE_DIR"

echo "[7/8] systemd unit"
install -m 0644 "$APP_DIR/deploy/minesweeper.service" /etc/systemd/system/minesweeper.service
sed -i \
  -e "s|__APP_DIR__|$APP_DIR|g" \
  -e "s|__STATE_DIR__|$STATE_DIR|g" \
  -e "s|__SERVICE_USER__|$SERVICE_USER|g" \
  -e "s|__PROFILE_URL__|$PROFILE_URL|g" \
  -e "s|__PORT__|$PORT|g" \
  -e "s|__MIN_CLICK_INTERVAL_S__|$MIN_CLICK_INTERVAL_S|g" \
  /etc/systemd/system/minesweeper.service
chmod a+rX "$APP_DIR" "$APP_DIR/_build" "$APP_DIR/_build/default" \
           "$APP_DIR/_build/default/bin"
chmod a+rx "$APP_DIR/_build/default/bin/main.exe"
chmod -R a+rX "$APP_DIR/assets"
systemctl daemon-reload
systemctl enable minesweeper
systemctl restart minesweeper
sleep 2
systemctl --no-pager status minesweeper | head -15

echo "[8/8] caddy"
install -m 0644 "$APP_DIR/deploy/Caddyfile" /etc/caddy/Caddyfile
sed -i "s|__DOMAIN__|$DOMAIN|g" /etc/caddy/Caddyfile
systemctl reload caddy

curl -sSf "http://localhost:$PORT/healthz"
