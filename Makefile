# RazvanME: hand made stuff :-)

all: lib exemples

lib:
	cd src && $(MAKE) lib

exemples:
	cd examples && $(MAKE)

clean:
	cd src && $(MAKE) clean
	cd examples && $(MAKE) clean
	rm -f *~

.PHONY: lib examples
