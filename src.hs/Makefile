all: lib

lib: ../lib.hs/Omega.o ../lib.hs/Omega.hi

../lib.hs/Omega.o ../lib.hs/Omega.hi: Omega.hs
	ghc -fglasgow-exts -c Omega.hs -odir ../lib.hs -hidir ../lib.hs

clean:
	rm -f *.hi *.o *~ ../lib.hs/*.hi ../lib.hs/*.o 
