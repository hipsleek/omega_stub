# $Id: Makefile,v 1.3 2003-05-16 12:48:47 raz Exp $

include ../Makefile.rules

all: lib exit

lib: ../lib/omega_stub.o

exit: ../lib/Exit.o

clean:
	rm -f *.o *~ ../lib/*.o

../lib/%.o: %.cpp
	$(CC) $(CFLAGS) $(INCS) -c $< -o ../lib/$@


../lib/Exit.o: $(BASEDIR)/basic/src/Exit.c
	$(CC) $(CFLAGS) $(INCS) -c $(BASEDIR)/basic/src/Exit.c -o $@


omega_stub.o: omega_stub.cpp ../include/omega_stub.h

