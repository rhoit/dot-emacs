#!/usr/bin/bash

set -v


if test -e repo; then
    (cd repo; git fetch --deepen 1; git reset --hard origin/master)
else
    # `--no-single-branch` to get all branches
    git clone --single-branch --depth=1 "git://git.savannah.gnu.org/emacs.git" repo
fi


makepkg --force --install
