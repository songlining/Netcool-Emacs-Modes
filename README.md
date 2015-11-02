# Netcool-Emacs-Modes

I used to work on some really messy rules files that were written by a group of people using different editors that resulted in different indentation and parenthesis opening styles (should you put **{** on the same line or next line of your **if** statement?).

My solution is to write my own major mode for the rules files and reformat them.  It made life a lot easier!

Want to see how it looks like? Check the [htmlized](http://emacswiki.org/emacs/htmlize.el) rules file below:

[The htmlized probe rules file](http://htmlpreview.github.io/?https://github.com/songlining/Netcool-Emacs-Modes/blob/master/snmptrap.rules.html)

Want to try it out?  Copy [the file](https://github.com/songlining/Netcool-Emacs-Modes/blob/master/rules-mode.el) into your local Emacs library directory (~/.emacs.d/lisp in my case) and add a line:

(require 'rules-mode)
