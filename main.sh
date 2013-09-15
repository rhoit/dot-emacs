#!/bin/bash

if [ -e ~/.emacs ]; then
    name=emacs$(date +%s).bak  
    mv ~/.emacs ~/$name
    echo "Old config has been renamed as $name"
fi

rm ~/.emacs -f
ln -s "$PWD/emacs.el" ~/.emacs


mkdir -p ~/.emacs.d/
rm -f ~/.emacs.d/plug-ins
ln -s "$PWD/plug-ins/" ~/.emacs.d/

rm -f ~/.emacs.d/repo
ln -s "$PWD/repo/" ~/.emacs.d/

echo "Linking plugin to .emacs.d"
