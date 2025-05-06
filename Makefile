EMACSDIR := ~/.emacs.d

.PHONY: test clean

test:
	emacs -q -l ./init.el --debug-init

clean:
	rm -f $(EMACSDIR)/early-init.el $(EMACSDIR)/early-init.elc $(EMACSDIR)/init.elc


