#!/usr/bin/env zsh

default_socket="/tmp/emacs-server/server"
if [[ -S "$default_socket" ]]; then
  emacsclient -t -c -s "$default_socket" --alternate-editor="" $@
else
  local pid
  local socket
  pid=$(pgrep -fo '(emacs.*daemon|Emacs\.app.*Emacs)$' | tail -1) &&
    test -n "$pid" &&
    socket=$(lsof -F n -aUp "$pid" 2>/dev/null | grep ^n/ | cut -c2- | awk '{print $1;}')
  if [[ -S "$socket" ]]; then
    emacsclient -t -c -s "$socket" --alternate-editor="" $@
  else
    echo >&2 "WARNING: emacs daemon not running, starting emacs..."
    emacs -nw $@
  fi
fi
