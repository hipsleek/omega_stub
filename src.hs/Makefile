all: lib

LIB_HS_DIR=../lib.hs

lib: $(LIB_HS_DIR)/Omega_stub.o $(LIB_HS_DIR)/Omega_stub.hi $(LIB_HS_DIR)/Omega.o $(LIB_HS_DIR)/Omega.hi

$(LIB_HS_DIR)/Omega_stub.o $(LIB_HS_DIR)/Omega_stub.hi: Omega_stub.hs
	ghc -fglasgow-exts -c Omega_stub.hs -odir $(LIB_HS_DIR) -hidir $(LIB_HS_DIR)

$(LIB_HS_DIR)/Omega.o $(LIB_HS_DIR)/Omega.hi: Omega.hs
	ghc -fglasgow-exts -i$(LIB_HS_DIR) -c Omega.hs -odir $(LIB_HS_DIR) -hidir $(LIB_HS_DIR)

clean:
	rm -f *.hi *.o *~ $(LIB_HS_DIR)/*.hi $(LIB_HS_DIR)/*.o 
