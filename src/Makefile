include ../Makefile.rules

all: lib

lib: omega_stub.o

clean:
	rm -f *.o *~ ../lib/omega_stub.o

%.o: %.cpp
	$(CC) $(CFLAGS) $(INCS) -c $<
	cp $@ ../lib

Exit.o: $(BASEDIR)/basic/src/Exit.c
	$(CC) $(CFLAGS) $(INCS) -c $(BASEDIR)/basic/src/Exit.c

omega_stub.o: omega_stub.cpp ../include/omega_stub.h

