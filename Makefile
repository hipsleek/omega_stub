# RazvanME: hand made stuff :-)
#
# $Id: Makefile,v 1.3 2003-05-25 03:07:58 raz Exp $

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
	cd examples.hs/example && $(MAKE) clean
	rm -f *~

.PHONY: lib lib.hs examples
