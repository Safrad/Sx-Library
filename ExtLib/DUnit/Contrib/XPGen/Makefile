#$Id: Makefile,v 1.6 2008/04/18 02:32:56 judc Exp $
ROOT=..\..

include $(ROOT)\Rules.mak

structure:
	$(DCC) -B -CC testXpgen.dpr 
	$(BIN_DIR)\testXpgen.exe

guitest:
	$(DCC) -B -CG testXpgen.dpr 
	$(BIN_DIR)\testXpgen.exe  
