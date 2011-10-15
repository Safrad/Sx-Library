#(@) $Id: Makefile,v 1.35 2008/04/18 01:10:23 judc Exp $
#
# This makefile was made to work with the cygwin version of 
# GNU Make. It was developed under Windows 2000, SP1.
#
# The use of GNU make's --win32 command-line switch is required.
#
# You can get a copy of the cygwin tools for Win32 at:
#   http://sources.redhat.com/cygwin/
#
#


# this is the version that will be used for the distribution file
VERSION=3.2.6.2

#this is the root directory, which will be overriden by sub-makefiles
ROOT=.

include Rules.mak

ZIPFILE=$(DIST_DIR)\dunit-$(VERSION).zip

DOC_FILES:=*.html *.htm *.hlp *.png *.gif *.jpg *.jpeg *.txt ChangeLog
PASCAL_FILES:=*.pas *.dpr *.inc *.dfm *.xfm *.res *.ini Makefile $(DOC_FILES)


dist: test compile_all bin doc zip 

nozipfile:
	-$(DEL) $(ZIPFILE)

zip: $(DIST_DIR) nozipfile zipdocs
	$(ZIP) -r $(ZIPFILE) $(SRC_DIR) $(TESTS_DIR) -i $(PASCAL_FILES)
	$(ZIP) -r $(ZIPFILE) examples\cmdline -i $(PASCAL_FILES) -x CVS
	$(ZIP) -r $(ZIPFILE) examples\collection -i $(PASCAL_FILES) -x CVS
	$(ZIP) -r $(ZIPFILE) examples\registration -i $(PASCAL_FILES) -x CVS
	$(ZIP) -r $(ZIPFILE) examples\registry -i $(PASCAL_FILES) -x CVS
	$(ZIP) -r $(ZIPFILE) examples\structure -i $(PASCAL_FILES) -x CVS
	$(ZIP) -r $(ZIPFILE) examples\TListTest -i $(PASCAL_FILES) -x CVS
	$(ZIP) -r $(ZIPFILE) examples\embeddable -i $(PASCAL_FILES) -x CVS
	$(ZIP) -r $(ZIPFILE) examples\testexception -i $(PASCAL_FILES) -x CVS
	$(ZIP) -r $(ZIPFILE) Contrib\XPGen -i $(PASCAL_FILES) -x CVS
	$(ZIP) $(ZIPFILE) Makefile $(DOC_FILES)
	$(ZIP) $(ZIPFILE) $(BIN_DIR)\UnitTests.exe $(BIN_DIR)\dunit.ini
	@$(ECHO) .

zipdocs: nozipfile
	$(ZIP) -r $(ZIPFILE) $(DOC_DIR) -i $(DOC_FILES) -x CVS
	
$(DIST_DIR):
	$(MKDIR) $(DIST_DIR) > nul

clean:
	$(DEL) scratch\* framework\*

fullchangelog:
	bash $(ROOT)\tools\cvs2cl.pl --file FullChangeLog  --tags --utc --window 604800 --prune --hide-filenames

changelogs:
	bash $(ROOT)\tools\cvs2cl.pl --distributed --tags --utc --window 604800 --hide-filenames --prune

compile_all: framework unit_tests examples contrib 

contrib: xpgen

framework: $(FWK_DIR)\DUnit.dcp

$(FWK_DIR)\DUnit.dcp: 
	@$(ECHO) .
	@$(ECHO) [ Building the Framework ]
	-$(MKDIR) $(FWK_DIR)
	@$(MAKE) --directory=$(SRC_DIR)
	$(COPY) $(SRC_DIR)\GUITestRunner.dfm $(FWK_DIR)
	@$(ECHO) .

test: framework unit_tests
	@$(ECHO) .
	@$(ECHO) [ Testing the Framework ]
	@$(MAKE) --directory=$(TESTS_DIR) test
	@$(ECHO) .

bin: framework
	@$(ECHO) .
	@$(ECHO) [ Building TestSuite Binary ]
	@$(MAKE) --directory=$(TESTS_DIR) bin
	$(COPY) $(TESTS_DIR)\dunit.ini $(BIN_DIR)\dunit.ini
	@$(ECHO) .


run: bin
	-$(BIN_DIR)\UnitTests.exe

doc:
	@$(ECHO) .
	@$(ECHO) [ Copying API Documentation ]
	@$(MAKE) --directory=./helpsrc	
	@$(ECHO) .

xpgen: framework
	@$(ECHO) [ Compiling Contrib: XPGen ]
	@$(MAKE) --directory=contrib/XPGen
	@$(ECHO) .

unit_tests: framework
	@$(ECHO) .
	@$(ECHO) [ Compiling Unit Tests ]
	@$(MAKE) --directory=$(TESTS_DIR)
	@$(ECHO) .

examples:  registration collection registry tlist structure cmdline

registration: framework
	@$(ECHO) [ Compiling Example: Registration ]
	@$(MAKE) --directory=examples/registration	
	@$(ECHO) .

collection: framework
	@$(ECHO) [ Compiling Example: Collection ]
	@$(MAKE) --directory=examples/collection
	@$(ECHO) .

registry: framework
	@$(ECHO) [ Compiling Example: Registry ]
	@$(MAKE) --directory=examples/registry
	@$(ECHO) .

tlist: framework
	@$(ECHO) [ Compiling Example: TListTest ]
	@$(MAKE) --directory=examples/TListTest
	@$(ECHO) .

structure: framework
	@$(ECHO) [ Compiling Example: Structure ]
	@$(MAKE) --directory=examples/structure
	@$(ECHO) .

cmdline: framework
	@$(ECHO) [ Compiling Example: CmdLine ]
	@$(MAKE) --directory=examples/cmdline
	@$(ECHO) .
