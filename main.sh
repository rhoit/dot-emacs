#!/bin/bash

if [ -e ~/.emacs ]; then
    name=emacs$(date +%s).bak  
    mv ~/.emacs ~/$name
fi

rm -f ~/.emacs
ln -s "$PWD/emacs" ~/.emacs
echo "Old config has been renamed as $name"

mkdir -p ~/.emacs.d/
rm -f ~/.emacs.d/plug-ins
ln -s "$PWD/plug-ins/" ~/.emacs.d/

rm -f ~/.emacs.d/repo
ln -s "$PWD/repo/" ~/.emacs.d/

echo "Linking plugin to .emacs.d"
