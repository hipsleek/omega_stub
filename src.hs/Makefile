all: lib

lib: Omega.o

Omega.o: Omega.hs
	ghc -fglasgow-exts -c Omega.hs

clean:
	rm -f *.hi *.o *~
