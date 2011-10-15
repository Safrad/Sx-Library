#$Id: Makefile,v 1.6 2008/04/18 02:33:00 judc Exp $
ROOT=..\..

include $(ROOT)\Rules.mak

structure:
	$(DCC) -B diffunit\Project1.dpr 
	$(DCC) -B -DTESTING diffunit\Project1Test.dpr
	$(DCC) -B sameunit\Project1.dpr 
	$(DCC) -B -DTESTING sameunit\Project1Test.dpr

