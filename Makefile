# RazvanME: hand made stuff :-)
#
# $Id: Makefile,v 1.2 2003-05-12 11:38:28 raz Exp $

all: lib exemples lib.hs

lib:
	cd src && $(MAKE) lib

lib.hs:
	cd src.hs && $(MAKE) lib

exemples:
	cd examples && $(MAKE)

clean:
	cd src && $(MAKE) clean
	cd src.hs && $(MAKE) clean
	cd examples && $(MAKE) clean
	rm -f *~

.PHONY: lib lib.hs examples
