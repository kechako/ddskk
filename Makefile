# Makefile: makefile for SKK.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>
# Version: $Id: Makefile,v 1.44.2.6 2001/09/11 12:23:15 czkmt Exp $
# Last Modified: $Date: 2001/09/11 12:23:15 $


VERSION = 11.5.0.1

BZIP2     = bzip2 -9
CP	  = /bin/cp -p
DATE	  = date
EMACS	  = emacs
NEMACS	  = nemacs
FLAGS     = -batch -q -no-site-file -l SKK-MK
GZIP      = gzip -9
MD5	  = md5
RM	  = /bin/rm -f
SNAPBASE  = ddskk-ne-`$(DATE) '+%Y%m%d'`
TAR	  = tar
XEMACS	  = xemacs
set_jisyo =

elc:
	$(NEMACS) $(FLAGS) -f SKK-MK-compile

package:
	$(XEMACS) $(FLAGS) -f SKK-MK-compile-package

info:
	$(EMACS) $(FLAGS) -f SKK-MK-compile-info

install:
	$(NEMACS) $(FLAGS) -f SKK-MK-install 

install-elc:
	$(EMACS) $(FLAGS) -f SKK-MK-install-elc

install-info:
	$(EMACS) $(FLAGS) -f SKK-MK-install-info

install-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-install-package 


what-where:
	$(NEMACS) $(FLAGS) -f SKK-MK-what-where

what-where-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-what-where-package

clean:
	-$(RM) leim-list.el skk-autoloads.el skk-setup.el *.elc experimental/*.elc \
	auto-autoloads.el custom-load.el \
	experimental/skk-isearch.el ./doc/skk.info* `find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'`

tar: clean
	cd .. ;\
	$(RM) ddskk-11.{1,2,3} ddskke18-$(VERSION) ddskk-snapshot ddskke18-$(VERSION).tar.gz ddskke18-$(VERSION).tar.bz2 ;\
	$(RM) ddskke18-$(VERSION) ;\
	ln -sf ddskk-e18 ddskke18-$(VERSION) ;\
	$(TAR) cvpf ddskke18-$(VERSION).tar --exclude-from=ddskke18-$(VERSION)/skk.ex --dereference ddskke18-$(VERSION) ;\
	$(BZIP2) -cf ddskke18-$(VERSION).tar > ddskke18-$(VERSION).tar.bz2 ;\
	$(GZIP) -cf ddskke18-$(VERSION).tar > ddskke18-$(VERSION).tar.gz ;\
	$(RM) ddskke18-$(VERSION).tar ;\
	$(RM) ddskke18-$(VERSION)

snapshot: clean
	cd .. ;\
	$(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot $(SNAPBASE).tar.gz $(SNAPBASE).tar.bz2 ;\
	$(RM) $(SNAPBASE) ;\
	ln -sf ddskk-e18 $(SNAPBASE) ;\
	$(TAR) cvpf $(SNAPBASE).tar --exclude-from=$(SNAPBASE)/skk.ex --dereference $(SNAPBASE);\
	$(BZIP2) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.bz2 ;\
	$(GZIP) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.gz ;\
	$(RM) $(SNAPBASE).tar ;\
	$(RM) $(SNAPBASE) ;\
	$(MD5) $(SNAPBASE).tar.bz2 >$(SNAPBASE).tar.bz2.md5 ;\
	$(MD5) $(SNAPBASE).tar.gz >$(SNAPBASE).tar.gz.md5
# end of Makefile.
