# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.13.4.5 2000/01/23 13:38:46 czkmt Exp $
# Last Modified: $Date: 2000/01/23 13:38:46 $

VERSION = 11.2

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

info:
	$(EMACS) $(FLAGS) -f SKK-MK-compile-info

install:
	$(EMACS) $(FLAGS) -f SKK-MK-install 

install-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-install-package 


what-where:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where

what-where-package:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where-package

clean:
	-$(RM) skk-autoloads.el *.elc experimental/*.elc ./doc/skk.info* `find . -name '*~'` 

tar: clean
	-$(RM) ../ddskk-$(VERSION).tar.gz
	cd .. ; ln -sf main ddskk-$(VERSION) ; \
           $(TAR) cvypf ddskk$(VERSION).tar.bz2 \
           --exclude-from=./ddskk-$(VERSION)/skk.ex --dereference ./ddskk-$(VERSION)

# end of Makefile.
