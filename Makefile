# RazvanME: hand made stuff :-)
#
# $Id: Makefile,v 1.6 2003-06-27 10:04:07 raz Exp $

all: lib lib.hs examples examples.hs

lib:
	cd src && $(MAKE) all

lib.hs:
	cd src.hs && $(MAKE) all

examples: lib lib.hs
	cd examples && $(MAKE)

examples.hs: lib lib.hs
	cd examples.hs/example && $(MAKE)
	cd examples.hs/myexample && $(MAKE)

clean:
	cd src && $(MAKE) clean
	cd src.hs && $(MAKE) clean
	cd examples && $(MAKE) clean
	cd examples.hs/example && $(MAKE) clean
	cd examples.hs/myexample && $(MAKE) clean
	rm -f *~

.PHONY: lib lib.hs examples
