# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.13.4.17 2000/09/10 09:46:51 minakaji Exp $
# Last Modified: $Date: 2000/09/10 09:46:51 $


VERSION = 11.3

TAR	= /usr/local/bin/tar
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK
BZIP2   = bzip2 -9
DATE	= date -u

set_jisyo =

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

package:
	$(XEMACS) $(FLAGS) -f SKK-MK-compile

info:
	$(EMACS) $(FLAGS) -f SKK-MK-compile-info

install:
	$(EMACS) $(FLAGS) -f SKK-MK-install 

install-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-install-package 


what-where:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where

what-where-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-what-where-package

clean:
	-$(RM) skk-autoloads.el skk-setup.el leim-list.el *.elc experimental/*.elc \
	auto-autoloads.el custom-load.el \
	experimental/skk-isearch.el ./doc/skk.info* `find . -name '*~'` `find . -name '.*~'`

tar: clean
	cd .. ; $(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot ddskk-`$(DATE) '+%Y%m%d'`.tar.bz ;\
	ln -sf main ddskk-$(VERSION) ;\
	$(TAR) cvpf ddskk-$(VERSION).tar --exclude-from=ddskk-$(VERSION)/skk.ex --dereference ddskk-$(VERSION) ;\
	$(BZIP2) -f ddskk-$(VERSION).tar

snapshot: clean
	cd .. ; -$(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot ddskk-`$(DATE) '+%Y%m%d'`.tar.bz ;\
	ln -sf main ddskk-`$(DATE) '+%Y%m%d'` ;\
	$(TAR) cvpf ddskk-`$(DATE) '+%Y%m%d'`.tar --exclude-from=ddskk-`$(DATE) '+%Y%m%d'`/skk.ex --dereference ddskk-`$(DATE) '+%Y%m%d'`;\
	$(BZIP2) -f ddskk-`$(DATE) '+%Y%m%d'`.tar ;\
	$(RM) ddskk-`$(DATE) '+%Y%m%d'`

# end of Makefile.
