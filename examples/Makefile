include ../Makefile.rules
all: library_example example

library_example: library_example.o Exit.o
	$(CC) $(CFLAGS) $(INCS) $(LIBS) library_example.o $(LIB) Exit.o -o library_example

example: example.o Exit.o ../lib/omega_stub.o
	$(CC) $(CFLAGS) $(INCS) $(LIBS) example.o ../lib/omega_stub.o $(LIB) Exit.o -o example

clean:
	rm -f *.o library_example example *~

%.o: %.c
	$(CC) $(CFLAGS) $(INCS) -c $<

Exit.o: $(BASEDIR)/basic/src/Exit.c
	$(CC) $(CFLAGS) $(INCS) -c $(BASEDIR)/basic/src/Exit.c

