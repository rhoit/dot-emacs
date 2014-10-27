#!/bin/bash

cd
for i in ".emacs*"; then
    name=$i$(date +%s).bak
    mv $i $name
    echo "Old config has been renamed as $name"
fi

cd -

ln -s $PWD ~/.emacs.d
echo "Linking plugin to .emacs.d"
