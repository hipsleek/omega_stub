# RazvanME: hand made stuff :-)
#
# $Id: Makefile,v 1.7 2003-07-28 05:07:18 raz Exp $

all: lib lib.hs examples.hs

lib:
	cd src && $(MAKE) all

lib.hs:
	cd src.hs && $(MAKE) all

examples: lib lib.hs
	cd examples && $(MAKE)

examples.hs: lib lib.hs
	cd examples.hs/example && $(MAKE)
	cd examples.hs/myexample && $(MAKE)
	cd examples.hs/append && $(MAKE)
	cd examples.hs/example1 && $(MAKE)
	cd examples.hs/example2 && $(MAKE)
	cd examples.hs/happy && $(MAKE)


clean:
	cd src && $(MAKE) clean
	cd src.hs && $(MAKE) clean
	cd examples && $(MAKE) clean
	cd examples.hs/example && $(MAKE) clean
	cd examples.hs/myexample && $(MAKE) clean
	cd examples.hs/append && $(MAKE) clean
	cd examples.hs/example1 && $(MAKE) clean
	cd examples.hs/example2 && $(MAKE) clean
	cd examples.hs/happy && $(MAKE) clean
	rm -f *~

.PHONY: lib lib.hs examples
