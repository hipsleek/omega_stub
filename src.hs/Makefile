all: lib

lib: ../lib.hs/Omega_stub.o ../lib.hs/Omega_stub.hi

../lib.hs/Omega_stub.o ../lib.hs/Omeg_stuba.hi: Omega_stub.hs
	ghc -fglasgow-exts -c Omega_stub.hs -odir ../lib.hs -hidir ../lib.hs

clean:
	rm -f *.hi *.o *~ ../lib.hs/*.hi ../lib.hs/*.o 
