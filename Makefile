# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.13.4.7 2000/03/11 02:17:15 minakaji Exp $
# Last Modified: $Date: 2000/03/11 02:17:15 $

VERSION = 11.2

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK
BZIP2   = bzip2

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
	-$(RM) skk-autoloads.el *.elc experimental/*.elc experimental/skk-isearch.el \
          ./doc/skk.info* `find . -name '*~'` `find . -name '.*~'` \
          ../ddskk*

tar: clean
	cd .. ; ln -sf main ddskk-$(VERSION)
	$(TAR) cvpf ../ddskk$(VERSION).tar --exclude-from=skk.ex --dereference ../ddskk-$(VERSION)
	$(BZIP2) ../ddskk$(VERSION).tar 

# end of Makefile.
