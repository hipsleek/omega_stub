include ../../Makefile.rules

all: example

example: Main.hs
	ghc -fglasgow-exts -i../../lib.hs -lstdc++ $(LIBS) $(LIB) ../../lib.hs/Omega_stub.o ../../lib/omega_stub.o ../../lib/Exit.o  Main.hs -o example

clean:
	rm -f *.hi *.o *~ example