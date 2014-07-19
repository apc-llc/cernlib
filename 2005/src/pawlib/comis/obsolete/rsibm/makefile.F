*
* $Id: makefile.F,v 1.1.1.1 1996/02/26 17:16:46 mclareni Exp $
*
* $Log: makefile.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:46  mclareni
* Comis
*
*
#include "sys/CERNLIB_machine.h"
#include "_comis/pilot.h"
#if defined(CERNLIB_NEVER)
/*CMZ :  1.18/02 13/04/94  17.43.12  by  Fons Rademakers*/
/*-- Author :    Vladimir Berezhnoi   01/04/94*/
# %W% revision of %E%  %U%
# This is an unpublished work copyright (c) 1992 Helios Software GmbH
# 3000 Hannover 1, West Germany

SHELL=/bin/sh
IPATH=
DEFS=
DEBUGFLAGS=-g -DDEBUG
NODEBUGFLAGS=-O
CFLAGS=$(IPATH) $(DEFS) $(NODEBUGFLAGS)
TARGETS=libdl.a
DEST=/usr/local/lib
HDRS=dlfcn.h
SRCS=dlfcn.c
OBJS=$(SRCS:%.c=%.o)

all:		$(TARGETS) dlfcn.c

$(TARGETS):	shr.o
		ar rv $@ $?

dlfcn.o:	dlfcn.h

shr.o:		$(OBJS) dl.exp
		$(CC) -o $@ $(OBJS) -bE:dl.exp -bM:SRE -e _nostart -lld

lint:
		lint $(IPATH) $(DEFS) $(SRCS) >lintout

info:
		sccs info

clean:
		rm -f lintout a.out core *.o *-lg *% *~ tags deps%

clobber:	clean
		rm -f $(TARGETS) deps

install:	all
		cp $(TARGETS) $(DEST)

shar:
		shar README Makefile dlfcn.h dlfcn.c dl.exp >dl.shar
#endif
