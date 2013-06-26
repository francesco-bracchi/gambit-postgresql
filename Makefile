GSC		=	gsc
GSI		=	gsi
LIBNAME		= 	postgresql
SRCDIR		= 	src
LIBDIR		= 	lib
TESTDIR		= 	test
EXAMPLESDIR	=	examples
DIR		=	ls
INSTALL		= 	cp
COPY		=	cp -r
MAKEDIR		=	mkdir
CC		=	gcc -shared

INSTALLDIR	= 	$(shell ${GSI} -e "(display (path-expand \"~~/${LIBNAME}\"))")
SOURCES		=	$(shell find ${SRCDIR} -name "*[a-zA-Z0-9].scm")
CFILES		=	$(SOURCES:.scm=.c)
OFILES		= 	$(CFILES:.c=.o)
LINKFILE	=	$(SRCDIR)/$(LIBNAME).o1
CLINKFILE	=	$(LINKFILE:.o1=.o1.c)
OLINKFILE	=	$(LINKFILE:.o1=.o1.o)
ILINKFILE	=	$(SRCDIR)/$(LIBNAME)\#.scm

INCLUDES	=	$(shell find ${SRCDIR} -name "*[\#].scm")
IDEST		= 	$(LIBDIR)/$(LIBNAME)\#.scm \
			$(LIBDIR)/connection\#.scm \
			$(LIBDIR)/exception\#.scm \
			$(LIBDIR)/commands/query\#.scm \
			$(LIBDIR)/commands/execute\#.scm \
			$(LIBDIR)/commands/notification\#.scm

all: libdir

clean: 
	-find -name "*.o1" | xargs rm 
	-find -name "*.c" | xargs rm 
	-find -name "*.o" | xargs rm 
	-find -name "*~" | xargs rm 
	-rm -r ${LIBDIR}
	-rm ${TESTDIR}/*.o1
	-rm ${EXAMPLESDIR}/*.o1

libdir: $(LINKFILE) $(LIBDIR) $(IDEST)
	$(COPY) $(LINKFILE) $(LIBDIR)
	$(COPY) $(ILINKFILE) $(LIBDIR)

$(LINKFILE): $(OLINKFILE) $(OFILES)
	$(CC) $(OFILES) $(OLINKFILE) -o $(LINKFILE)

$(CLINKFILE):
	$(GSC) -link -flat -o $(CLINKFILE) $(SOURCES)

$(OLINKFILE):  $(CLINKFILE)
	$(GSC) -cc-options "-D___DYNAMIC" -obj -o $(OLINKFILE) $(CLINKFILE)

%.o: %.c
	$(GSC) -cc-options "-D___DYNAMIC" -obj -o $@ $<

%.c: %.scm
	$(GSC -c -o $@ $<

$(LIBDIR)/%.scm: $(SRCDIR)/%.scm $(LIBDIR)
	$(COPY) $< $@

$(LIBDIR):
	-$(MAKEDIR) $(LIBDIR)
	-$(MAKEDIR) $(LIBDIR)/commands
	-$(MAKEDIR) $(LIBDIR)/messages
	-$(MAKEDIR) $(LIBDIR)/utils

$(INSTALLDIR):
	-$(MAKEDIR) $(INSTALLDIR)

install: libdir $(INSTALLDIR)
	@echo "installing in:"
	@echo $(INSTALLDIR)
	$(INSTALL) -r $(LIBDIR)/* $(INSTALLDIR)

repl: libdir
	@echo "testing repl"
	$(GSI) -:~~$(LIBNAME)=$(LIBDIR) $(LIBDIR)/$(LIBNAME) - # $(EXAMPLESDIR)/repl
