# The Makefile
#

MyBot:
	sbcl  --end-runtime-options --no-sysinit --no-userinit --disable-debugger --load MyBot.lisp --eval "(save-lisp-and-die \"MyBot\" :executable t :toplevel #'pwbot::main)"


entry.zip: MyBot.lisp planetwars.lisp
	rm -f $@
	git archive --format zip -o $@ HEAD $^

.PHONY submission clean

submission: entry.zip

clean:
	rm -f *.fasl MyBot entry.zip

