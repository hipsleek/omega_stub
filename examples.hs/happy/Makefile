include ../../Makefile.rules

all: parser

parser: Main.hs Omega_parser.o Omega_parser.hi
	ghc -fglasgow-exts -i../../lib.hs -lstdc++ $(LIBS) $(LIB)	\
		../../lib.hs/Omega_stub.o				\
		../../lib.hs/Omega_util.o				\
		../../lib.hs/Omega.o					\
		../../lib.hs/Omega_types.o				\
		../../lib/omega_stub.o					\
		../../lib/Exit.o Omega_tokens.o				\
		Omega_lexer.o						\
		Omega_parser.o						\
		Main.hs							\
		-o parser

Omega_parser.o Omega_parser.hi: Omega_parser.hs Omega_lexer.hi Omega_tokens.hi
	ghc -fglasgow-exts -i../../lib.hs -lstdc++ $(LIBS) $(LIB)	\
		../../lib.hs/Omega_stub.o				\
		../../lib.hs/Omega_util.o				\
		../../lib.hs/Omega.o					\
		../../lib.hs/Omega_types.o				\
		../../lib/omega_stub.o					\
		Omega_tokens.o						\
		Omega_lexer.o						\
		../../lib/Exit.o					\
		-c Omega_parser.hs

Omega_parser.hs: Omega_parser.y 
	happy -i Omega_parser.y

%.hi: %.hs
	ghc -fglasgow-exts -i../../lib.hs -lstdc++ $(LIBS) $(LIB) 	\
		../../lib/omega_stub.o					\
		../../lib/Exit.o					\
		../../lib.hs/Omega_stub.o				\
		../../lib.hs/Omega_util.o				\
		../../lib.hs/Omega.o					\
		../../lib.hs/Omega_types.o				\
		-c $<

#	ghc -fglasgow-exts -i../../lib.hs -lstdc++ $(LIBS) $(LIB) ../../lib.hs/Omega_stub.o ../../lib.hs/Omega_util.o ../../lib.hs/Omega.o ../../lib/omega_stub.o ../../lib/Exit.o -c $< 

clean:
	rm -f *.hi *.o *.info *~ example1 parser Omega_parser.hs

Omega_lexer.hs: Omega_tokens.hi
