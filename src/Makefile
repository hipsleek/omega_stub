# $Id: Makefile,v 1.5 2003-07-22 07:52:39 raz Exp $

include ../Makefile.rules

all: lib exit ../lib/Exit.o

lib: ../lib/omega_stub.o

exit: ../lib/Exit.o

clean:
	rm -f *.o *~ ../lib/*.o

../lib/Exit.o: $(BASEDIR)/basic/src/Exit.c
	$(CC) $(CFLAGS) $(INCS) -c $(BASEDIR)/basic/src/Exit.c -o $@


../lib/omega_stub.o: omega_stub.cpp ../include/omega_stub.h ../include/util.h
	$(CC) $(CFLAGS) $(INCS) -c $< -o ../lib/$@

#../lib/%.o: %.cpp
#	$(CC) $(CFLAGS) $(INCS) -c $< -o ../lib/$@
