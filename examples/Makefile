include ../Makefile.rules
all: library_example example

library_example: library_example.o ../lib/Exit.o
	$(CC) $(CFLAGS) $(INCS) $(LIBS) library_example.o $(LIB) ../lib/Exit.o -o library_example

example: example.o ../lib/Exit.o ../lib/omega_stub.o
	$(CC) $(CFLAGS) $(INCS) $(LIBS) example.o ../lib/omega_stub.o $(LIB) ../lib/Exit.o -o example

clean:
	rm -f *.o library_example example *~

%.o: %.c
	$(CC) $(CFLAGS) $(INCS) -c $<

