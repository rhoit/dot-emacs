#!/usr/bin/bash

if test -e repo; then
    echo "    updating ... "
    (cd repo; git fetch --deepen 1; git reset --hard origin/master)
else
    git clone --no-single-branch --depth=1 "git://git.savannah.gnu.org/emacs.git" repo
fi

makepkg --force --install
