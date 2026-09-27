#!/usr/bin/env bash

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")" && pwd -P)"
EMACS_CONFIG="$HOME/.config/emacs"
CONFIG_DIR="$HOME/.config"

mkdir -p "$CONFIG_DIR"

if [[ -L "$EMACS_CONFIG" ]]; then
  TARGET="$(readlink "$EMACS_CONFIG")"
  if [[ "$TARGET" == "$REPO_DIR" ]]; then
    echo "✓ Emacs config already linked to this repo ($EMACS_CONFIG → $REPO_DIR)"
    exit 0
  else
    echo "⚠ $EMACS_CONFIG is already a symlink pointing to: $TARGET"
    read -p "Replace it to point to this repo? (y/N) " -r response
    if [[ ! "${response,,}" =~ ^y ]]; then
      echo "Cancelled."
      exit 1
    fi
    rm "$EMACS_CONFIG"
  fi
fi

if [[ -e "$EMACS_CONFIG" ]]; then
  BACKUP="$EMACS_CONFIG.bak.$(date +%s)"
  echo "→ Backing up existing $EMACS_CONFIG to $BACKUP"
  mv "$EMACS_CONFIG" "$BACKUP"
fi

ln -s "$REPO_DIR" "$EMACS_CONFIG"
echo "✓ Linked $EMACS_CONFIG → $REPO_DIR"
echo "  Packages will auto-install when you start Emacs."
