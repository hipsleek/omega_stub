include ../../Makefile.rules

all: parser

parser: Main.hs 
	ghc -fglasgow-exts -i../../lib.hs -lstdc++ $(LIBS) $(LIB)	\
		../../lib.hs/Omega_stub.o				\
		../../lib.hs/Omega_util.o				\
		../../lib.hs/Omega.o					\
		../../lib.hs/Omega_types.o				\
		../../lib/omega_stub.o					\
		../../lib/Exit.o					\
		../../lib.hs/Omega_tokens.o				\
		../../lib.hs/Omega_lexer.o				\
		../../lib.hs/Omega_parser.o				\
		Main.hs							\
		-o parser

clean:
	rm -f *.hi *.o *.info *~ example1 parser Omega_parser.hs

Omega_lexer.hs: Omega_tokens.hi
