install:
	mv ~/.emacs ~/.emacs$(date +%s).bak
	ln -s ${PWD} ~/.emacs.d
