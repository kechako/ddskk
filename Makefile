# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.13.4.9 2000/03/13 08:43:44 kawamura Exp $
# Last Modified: $Date: 2000/03/13 08:43:44 $

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
	./doc/skk.info* `find . -name '*~'` `find . -name '.*~'`

tar: clean
	-$(RM) ../ddskk*
	cd .. ; ln -sf main ddskk-$(VERSION)
	$(TAR) cvpf ../ddskk$(VERSION).tar --exclude-from=skk.ex --dereference ../ddskk-$(VERSION)
	$(BZIP2) ../ddskk$(VERSION).tar 

snapshot: clean
	-$(RM) ../ddskk*
	cd .. ; ln -sf main ddskk-snapshot
	$(TAR) cvpf ../ddskk-snapshot.tar --exclude-from=skk.ex --dereference ../ddskk-snapshot
	$(BZIP2) ../ddskk-snapshot.tar 

# end of Makefile.
