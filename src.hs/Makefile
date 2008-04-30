all: lib

#LIB_HS_DIR=../lib.prof.hs
#GHC_FLAGS=-prof -fglasgow-exts -fallow-overlapping-instances 

LIB_HS_DIR=../lib.hs
GHC_FLAGS= -O2 -fglasgow-exts -fallow-overlapping-instances 

lib:						\
	$(LIB_HS_DIR)/Omega_util.o		\
	$(LIB_HS_DIR)/Omega_util.hi		\
	$(LIB_HS_DIR)/Omega_stub.o		\
	$(LIB_HS_DIR)/Omega_stub.hi		\
	$(LIB_HS_DIR)/Omega.o			\
	$(LIB_HS_DIR)/Omega.hi			\
	$(LIB_HS_DIR)/PFOmega.o			\
	$(LIB_HS_DIR)/PFOmega.hi

$(LIB_HS_DIR)/Omega_util.o $(LIB_HS_DIR)/Omega_util.hi: Omega_util.hs
$(LIB_HS_DIR)/Omega_stub.o $(LIB_HS_DIR)/Omega_stub.hi: Omega_stub.hs
$(LIB_HS_DIR)/Omega.o $(LIB_HS_DIR)/Omega.hi: Omega.hs  $(LIB_HS_DIR)/Omega_stub.hi $(LIB_HS_DIR)/Omega_types.hi $(LIB_HS_DIR)/Omega_parser.hi
$(LIB_HS_DIR)/Omega_types.o $(LIB_HS_DIR)/Omega_types.hi: Omega_types.hs $(LIB_HS_DIR)/Omega_util.hi $(LIB_HS_DIR)/Omega_stub.hi
$(LIB_HS_DIR)/Omega_parser.o $(LIB_HS_DIR)/Omega_parser.hi: Omega_parser.hs $(LIB_HS_DIR)/Omega_util.hi $(LIB_HS_DIR)/Omega_types.hi $(LIB_HS_DIR)/Omega_lexer.hi
$(LIB_HS_DIR)/Omega_stub.o $(LIB_HS_DIR)/Omega_stub.hi: Omega_stub.hs

$(LIB_HS_DIR)/PFOmega.o $(LIB_HS_DIR)/PFOmega.hi: PFOmega.hs  $(LIB_HS_DIR)/Omega_stub.hi $(LIB_HS_DIR)/Omega_types.hi $(LIB_HS_DIR)/Omega_parser.hi $(LIB_HS_DIR)/Omega.hi

$(LIB_HS_DIR)/%.hi: %.hs
	ghc $(GHC_FLAGS) -i$(LIB_HS_DIR) -c $< -odir $(LIB_HS_DIR) -hidir $(LIB_HS_DIR)
$(LIB_HS_DIR)/%.o: %.hs
	ghc $(GHC_FLAGS) -i$(LIB_HS_DIR) -c $< -odir $(LIB_HS_DIR) -hidir $(LIB_HS_DIR)

Omega_parser.hs: Omega_parser.y $(LIB_HS_DIR)/Omega_tokens.hi $(LIB_HS_DIR)/Omega_types.hi
	happy -i Omega_parser.y

clean:
	rm -f *.hi *.o *~ $(LIB_HS_DIR)/*.hi $(LIB_HS_DIR)/*.o *.info 
