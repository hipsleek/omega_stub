all: lib

LIB_HS_DIR=../lib.hs
GHCFLAGS=-fglasgow-exts -fallow-overlapping-instances

lib: $(LIB_HS_DIR)/Omega_util.o $(LIB_HS_DIR)/Omega_stub.o $(LIB_HS_DIR)/Omega_stub.hi $(LIB_HS_DIR)/Omega.o $(LIB_HS_DIR)/Omega.hi

$(LIB_HS_DIR)/Omega_util.o $(LIB_HS_DIR)/Omega_util.hi: Omega_util.hs
	ghc $(GHCFLAGS) -c Omega_util.hs -odir $(LIB_HS_DIR) -hidir $(LIB_HS_DIR)

$(LIB_HS_DIR)/Omega_stub.o $(LIB_HS_DIR)/Omega_stub.hi: Omega_stub.hs
	ghc $(GHCFLAGS) -c Omega_stub.hs -odir $(LIB_HS_DIR) -hidir $(LIB_HS_DIR)

$(LIB_HS_DIR)/Omega.o $(LIB_HS_DIR)/Omega.hi: Omega.hs
	ghc $(GHCFLAGS) -i$(LIB_HS_DIR) -c Omega.hs -odir $(LIB_HS_DIR) -hidir $(LIB_HS_DIR)

clean:
	rm -f *.hi *.o *~ $(LIB_HS_DIR)/*.hi $(LIB_HS_DIR)/*.o 
