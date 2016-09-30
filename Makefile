install:
	! test -h ~/.emacs.d  || rm -f ~/.emacs.d
	! test -d ~/.emacs.d  || mv ~/.emacs.d ~/.emacs.d$(date +%s).bak
	! test -e ~/.emacs.el || mv ~/.emacs.el ~/.emacs.el$(date +%s).bak
	ln -s ${PWD} ~/.emacs.d
