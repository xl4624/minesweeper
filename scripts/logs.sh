#!/usr/bin/env bash
# Fetch or tail the minesweeper log from the deploy host.
#
# Reads DEPLOY_HOST, DEPLOY_USER, DEPLOY_SSH_KEY from .env at the repo root.
#
# Usage:
#   scripts/logs.sh [pull]   # default: copies mines.log{,.old} into ./logs/
#   scripts/logs.sh tail     # stream the live log over SSH

set -euo pipefail

cd "$(dirname "$0")/.."

if [ -f .env ]; then
  set -a; . ./.env; set +a
fi

: "${DEPLOY_HOST:?Set DEPLOY_HOST in .env}"
: "${DEPLOY_USER:?Set DEPLOY_USER in .env}"

SSH_KEY="${DEPLOY_SSH_KEY:-}"
SSH_KEY="${SSH_KEY/#\~/$HOME}"

REMOTE_DIR="/var/lib/minesweeper"
DEST="${DEPLOY_USER}@${DEPLOY_HOST}"

ssh_opts=()
[ -n "$SSH_KEY" ] && ssh_opts+=(-i "$SSH_KEY")

case "${1:-pull}" in
  pull)
    mkdir -p logs
    for f in mines.log mines.log.old; do
      if scp -q ${ssh_opts[@]+"${ssh_opts[@]}"} "$DEST:$REMOTE_DIR/$f" "logs/$f" 2>/dev/null; then
        echo "  logs/$f ($(wc -l < "logs/$f") lines)"
      else
        echo "  logs/$f (not present on host)"
      fi
    done
    ;;
  tail)
    exec ssh ${ssh_opts[@]+"${ssh_opts[@]}"} "$DEST" "tail -F $REMOTE_DIR/mines.log"
    ;;
  *)
    echo "Usage: $0 [pull|tail]" >&2
    exit 2
    ;;
esac
