#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: install-open-with-emacs-linux.sh [--no-defaults] [--daemon-only] [--help]

Installs a per-user Emacs Client desktop entry and, unless --no-defaults is
used, makes it the default handler for MIME types listed in mime-types.txt.
Also installs/enables a per-user systemd Emacs daemon service when available.

Options:
  --no-defaults   Install desktop integration but do not change xdg-mime defaults
  --daemon-only   Only install/enable the Emacs daemon service
  --help          Show this help
EOF
}

set_defaults=1
daemon_only=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --no-defaults) set_defaults=0 ;;
    --daemon-only) daemon_only=1 ;;
    --help|-h) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

script_dir="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
mime_file="$script_dir/mime-types.txt"
desktop_id="emacs-client-open.desktop"
applications_dir="${XDG_DATA_HOME:-$HOME/.local/share}/applications"
desktop_target="$applications_dir/$desktop_id"
systemd_user_dir="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"
service_target="$systemd_user_dir/emacs.service"

read_mime_types() {
  if [ ! -f "$mime_file" ]; then
    echo "Missing MIME type file: $mime_file" >&2
    exit 1
  fi
  grep -E -v '^[[:space:]]*($|#)' "$mime_file" | sed 's/[[:space:]]//g' || true
}

systemd_quote() {
  local value="$1"
  value="${value//%/%%}"
  value="${value//\\/\\\\}"
  value="${value//\"/\\\"}"
  printf '"%s"' "$value"
}

install_desktop_entry() {
  mkdir -p "$applications_dir"
  local mime_types mime_line
  mime_types="$(read_mime_types | paste -sd ';' -)"
  if [ -n "$mime_types" ]; then
    mime_line="${mime_types};"
  else
    echo "No MIME types found in $mime_file" >&2
    exit 1
  fi

  cat > "$desktop_target" <<EOF
[Desktop Entry]
Name=Emacs Client
GenericName=Text Editor
Comment=Edit text and code files in Emacs client frame
MimeType=$mime_line
Exec=emacsclient -c -n -a emacs %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupNotify=true
EOF

  if command -v update-desktop-database >/dev/null 2>&1; then
    update-desktop-database "$applications_dir" || true
  fi

  if [ "$set_defaults" -eq 1 ]; then
    if command -v xdg-mime >/dev/null 2>&1; then
      while IFS= read -r mime; do
        [ -n "$mime" ] || continue
        xdg-mime default "$desktop_id" "$mime"
      done < <(read_mime_types)
    else
      echo "xdg-mime not found; skipped default MIME handler setup." >&2
    fi
  fi

  echo "Installed desktop entry: $desktop_target"
  if [ "$set_defaults" -eq 1 ]; then
    echo "Updated xdg-mime defaults for MIME types in: $mime_file"
  else
    echo "Skipped changing xdg-mime defaults (--no-defaults)."
  fi
}

install_daemon_service() {
  if ! command -v systemctl >/dev/null 2>&1; then
    echo "systemctl not found; daemon service skipped. emacsclient fallback still uses '-a emacs'." >&2
    return 0
  fi

  local emacs_path emacsclient_path tmp_service backup_target
  emacs_path="$(command -v emacs || true)"
  emacsclient_path="$(command -v emacsclient || true)"

  if [ -z "$emacs_path" ] || [ -z "$emacsclient_path" ]; then
    echo "emacs and emacsclient must be available on PATH to install the daemon service." >&2
    return 1
  fi

  mkdir -p "$systemd_user_dir"
  tmp_service="$(mktemp)"
  cat > "$tmp_service" <<EOF
[Unit]
Description=Emacs text editor daemon
Documentation=info:emacs man:emacs(1)

[Service]
Type=forking
ExecStart=$(systemd_quote "$emacs_path") --daemon
ExecStop=$(systemd_quote "$emacsclient_path") --eval "(kill-emacs)"
Restart=on-failure

[Install]
WantedBy=default.target
EOF

  if [ -f "$service_target" ] && ! cmp -s "$tmp_service" "$service_target"; then
    backup_target="$service_target.backup.$(date +%Y%m%d%H%M%S)"
    cp "$service_target" "$backup_target"
    echo "Backed up existing service to: $backup_target"
  fi
  mv "$tmp_service" "$service_target"

  systemctl --user import-environment \
    PATH XDG_DATA_DIRS XDG_CONFIG_HOME XDG_DATA_HOME \
    DISPLAY WAYLAND_DISPLAY XAUTHORITY DBUS_SESSION_BUS_ADDRESS || true

  if systemctl --user daemon-reload && systemctl --user enable --now emacs.service; then
    echo "Installed and enabled user service: $service_target"
  else
    echo "Wrote $service_target but could not enable/start it with systemctl --user." >&2
    echo "The desktop entry still works through: emacsclient -c -n -a emacs" >&2
  fi
}

if [ "$daemon_only" -eq 0 ]; then
  install_desktop_entry
fi
install_daemon_service

cat <<EOF

Verification:
  emacsclient -c -n -a emacs README.org
  xdg-mime query default text/plain
  xdg-open README.org
  systemctl --user status emacs.service
EOF
