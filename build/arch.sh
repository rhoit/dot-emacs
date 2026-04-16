#!/usr/bin/bash

set -v


if test -e repo; then
    (cd repo; git fetch --deepen 1; git reset --hard origin/master)
else
    # `--no-single-branch` to get all branches
    # git clone --single-branch --depth=1 "git://git.savannah.gnu.org/emacs.git" repo
    git clone --single-branch --depth=1 "https://github.com/emacs-mirror/emacs.git" repo
fi

# pkg-config --list-all  # run if dependencies installed but not found
makepkg --syncdeps --force --install
