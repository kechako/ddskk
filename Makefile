# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.13.4.1 1999/11/14 15:10:07 minakaji Exp $
# Last Modified: $Date: 1999/11/14 15:10:07 $

VERSION = 11.1

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

install:
	$(EMACS) $(FLAGS) -f SKK-MK-install 

install-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-install-package 


what-where:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where

what-where-package:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where-package

clean:
	-$(RM) *.elc ./doc/skk.info* `find . -name '*~'` 

tar:
	-$(RM) *.elc ./doc/skk.info* ../ddskk-$(VERSION).tar.gz \
           `find . -name '*~'` `find . -name '*~'`
	cd .. ; ln -sf main ddskk-$(VERSION) ; \
           $(TAR) cvypf ddskk$(VERSION).tar.bz2 \
           --exclude-from=./ddskk-$(VERSION)/skk.ex --dereference ./ddskk-$(VERSION)

# end of Makefile.
