include ../../Makefile.rules

all: append

append: Main.hs
	ghc -fglasgow-exts -i../../lib.hs -lstdc++ $(LIBS) $(LIB) 	\
		../../lib/omega_stub.o					\
		../../lib/Exit.o					\
		../../lib.hs/Omega_stub.o				\
		../../lib.hs/Omega_util.o				\
		../../lib.hs/Omega.o					\
		../../lib.hs/Omega_types.o				\
		../../lib.hs/Omega_tokens.o				\
		../../lib.hs/Omega_lexer.o				\
		../../lib.hs/Omega_parser.o				\
		Main.hs							\
		-o append

clean:
	rm -f *.hi *.o *~ append