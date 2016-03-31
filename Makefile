install:
	mv ~/.emacs ~/.emacs$(date +%s).bak
	mv ~/.emacs.el ~/.emacs.el$(date +%s).bak
	ln -s ${PWD} ~/.emacs.d
