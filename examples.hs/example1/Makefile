include ../../Makefile.rules

all: example

example: Main.hs
	ghc -fglasgow-exts -i../../lib.hs -lstdc++ $(LIBS) $(LIB) ../../lib.hs/Omega_stub.o ../../lib.hs/Omega_util.o ../../lib.hs/Omega.o ../../lib/omega_stub.o ../../lib/Exit.o  Main.hs -o example1

clean:
	rm -f *.hi *.o *~ example