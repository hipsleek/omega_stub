# RazvanME: hand made stuff :-)
#
# $Id: Makefile,v 1.4 2003-05-26 05:11:37 raz Exp $

all: lib lib.hs exemples

lib:
	cd src && $(MAKE) all

lib.hs:
	cd src.hs && $(MAKE) all

exemples: lib lib.hs
	cd examples && $(MAKE)

clean:
	cd src && $(MAKE) clean
	cd src.hs && $(MAKE) clean
	cd examples && $(MAKE) clean
	cd examples.hs/example && $(MAKE) clean
	rm -f *~

.PHONY: lib lib.hs examples
