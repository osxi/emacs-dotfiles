#!/usr/bin/env bash
# Bootstrap script: symlinks this config repo to ~/.config/emacs
# Safely handles existing configs, symlinks, and makes the setup idempotent.

set -euo pipefail

# Resolve the repo's absolute path (portable: works on macOS and Linux)
REPO_DIR="$(cd "$(dirname "$0")" && pwd -P)"
EMACS_CONFIG="$HOME/.config/emacs"
CONFIG_DIR="$HOME/.config"

# Ensure ~/.config exists
mkdir -p "$CONFIG_DIR"

# Case 1: Already a symlink pointing to this repo
if [[ -L "$EMACS_CONFIG" ]]; then
  TARGET="$(readlink "$EMACS_CONFIG")"
  if [[ "$TARGET" == "$REPO_DIR" ]]; then
    echo "✓ Emacs config already linked to this repo ($EMACS_CONFIG → $REPO_DIR)"
    exit 0
  else
    # Symlink exists but points elsewhere
    echo "⚠ $EMACS_CONFIG is already a symlink pointing to: $TARGET"
    read -p "Replace it to point to this repo? (y/N) " -r response
    if [[ ! "${response,,}" =~ ^y ]]; then
      echo "Cancelled."
      exit 1
    fi
    rm "$EMACS_CONFIG"
  fi
fi

# Case 2: Exists as a real file/directory (need to back it up)
if [[ -e "$EMACS_CONFIG" ]]; then
  BACKUP="$EMACS_CONFIG.bak.$(date +%s)"
  echo "→ Backing up existing $EMACS_CONFIG to $BACKUP"
  mv "$EMACS_CONFIG" "$BACKUP"
fi

# Case 3: Doesn't exist — just symlink
ln -s "$REPO_DIR" "$EMACS_CONFIG"
echo "✓ Linked $EMACS_CONFIG → $REPO_DIR"
echo "  Packages will auto-install when you start Emacs."
