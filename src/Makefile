# $Id: Makefile,v 1.2 2003-05-12 11:38:38 raz Exp $

include ../Makefile.rules

all: lib exit

lib: omega_stub.o

exit: Exit.o

clean:
	rm -f *.o *~ ../lib/omega_stub.o

%.o: %.cpp
	$(CC) $(CFLAGS) $(INCS) -c $<
	cp $@ ../lib

Exit.o: $(BASEDIR)/basic/src/Exit.c
	$(CC) $(CFLAGS) $(INCS) -c $(BASEDIR)/basic/src/Exit.c
	cp $@ ../lib

omega_stub.o: omega_stub.cpp ../include/omega_stub.h

