#$Id: Makefile,v 1.6 2008/04/18 02:33:03 judc Exp $
ROOT=..

include $(ROOT)\\Rules.mak

bin:
	-$(MKDIR) $(BIN_DIR)
	$(DCC) -B -E$(BIN_DIR) UnitTests.dpr

test:
	$(DCC) -CC -B UnitTests.dpr
	$(BIN_DIR)\\UnitTests.exe -text-mode
