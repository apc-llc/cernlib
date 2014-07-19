*
* $Id: aixdld.F,v 1.1.1.1 1996/02/26 17:16:46 mclareni Exp $
*
* $Log: aixdld.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:46  mclareni
* Comis
*
*
#include "sys/CERNLIB_machine.h"
#include "_comis/pilot.h"
#if defined(CERNLIB_NEVER)
/*CMZ :  1.18/02 13/04/94  17.41.06  by  Fons Rademakers*/
/*-- Author :    Vladimir Berezhnoi   30/03/94*/
#!/bin/sh
# This is a shell archive (shar 3.24)
# made 05/25/1993 16:40 UTC by jum@osiris
# Source directory /home/jum/dyn/rs6000
#
# existing files WILL be overwritten
#
# This shar contains:
# length  mode       name
# ------ ---------- ------------------------------------------
#   4062 -rw-rw-r-- README
#    745 -rw-rw-r-- Makefile
#    696 -r--r--r-- dlfcn.h
#  10486 -r--r--r-- dlfcn.c
#     54 -rw-rw-r-- dl.exp
#
if touch 2>&1 | fgrep '[-amc]' > /dev/null
 then TOUCH=touch
 else TOUCH=true
fi
# ============= README ==============
echo "x - extracting README (Text)"
sed 's/^X//' << 'SHAR_EOF' > README &&
XCopyright (c) 1992,1993, Jens-Uwe Mager, Helios Software GmbH
XNot derived from licensed software.
X
XPermission is granted to freely use, copy, modify, and redistribute
Xthis software, provided that no attempt is made to gain profit from it,
Xthe author is not construed to be liable for any results of using the
Xsoftware, alterations are clearly marked as such, and this notice is
Xnot modified.
X
Xlibdl.a
X-------
X
XThis is an emulation library to emulate the SunOS/System V.4 functions
Xto access the runtime linker. The functions are emulated by using the
XAIX load() function and by reading the .loader section of the loaded
Xmodule to find the exports. The to be loaded module should be linked as
Xfollows:
X
X	cc -o module.so -bM:SRE -bE:module.exp -e _nostart $(OBJS)
X
XThe module export file contains the symbols to be exported. Because
Xthis library uses the loader section, the final module.so file can be
Xstripped.
X
XUsage
X-----
X
Xvoid *dlopen(const char *path, int mode);
X
XThis routine loads the module pointed to by path and reads its export
Xtable. If the path does not contain a '/' character, dlopen will search
Xfor the module using the LIBPATH environment variable. It returns an
Xopaque handle to the module or NULL on error. The flags parameter is
Xcurrently ignored.
X
X
Xvoid *dlsym(void *handle, const char *symbol);
X
XThis routine searches for the symbol in the module referred to by
Xhandle and returns its address. If the symbol could not be found, the
Xfunction returns NULL. The return value must be casted to a proper
Xfunction pointer before it can be used. SunOS/System V.4 allow handle
Xto be a NULL pointer to refer to the module the call is made from, this
Xis not implemented.
X
Xint dlclose(void *handle);
X
XThis routine unloads the module referred to by the handle and disposes
Xof any local storage. this function returns -1 on failure.
X
Xchar *dlerror(void);
X
XThis routine can be used to retrieve a text message describing the most
Xrecent error that occured on on of the above routines. This function
Xreturns NULL if there is not error information.
X
XInitialization and termination handlers
X---------------------------------------
X
XThe emulation provides for an initialization and a termination
Xhandler.  The dlfcn.h file contains a structure declaration named
Xdl_info with following members:
X
X	void (*init)(void);
X	void (*fini)(void);
X
XThe init function is called upon first referencing the library. The
Xfini function is called at dlclose() time or when the process exits.
XThe module should declare a variable named dl_info that contains this
Xstructure which must be exported.  These functions correspond to the
Xdocumented _init() and _fini() functions of SunOS 4.x, but these are
Xappearently not implemented in SunOS.  When using SunOS 5.0, these
Xcorrespond to #pragma init and #pragma fini respectively.
X
XJens-Uwe Mager
X
XHELIOS Software GmbH
XLavesstr. 80
X3000 Hannover 1
XGermany
X
XPhone:		+49 511 3681093
XFAX:		+49 511 3681095
XAppleLink:	ger.xse0082	Attn: Jens-Uwe Mager
Xuucp:		jum@helios.de	or heliosd!jum
X
XRevision history:
X-----------------
X
XSCCS/s.dlfcn.c:
X
XD 1.4 93/01/03 19:13:56 jum 4 3	00061/00005/00403
XMRs:
XCOMMENTS:
Xto allow calling symbols in the main module call load with L_NOAUTODEFER and
Xdo a loadbind later with the main module.
X
XD 1.3 92/12/27 20:59:55 jum 3 2	00066/00008/00342
XMRs:
XCOMMENTS:
Xadded search by L_GETINFO if module got loaded by LIBPATH
X
XD 1.2 92/08/16 17:45:43 jum 2 1	00074/00006/00276
XMRs:
XCOMMENTS:
Ximplemented initialize and terminate functions, added reference counting to
avoid multiple loads of the same library
X
XD 1.1 92/08/02 18:08:45 jum 1 0	00282/00000/00000
XMRs:
XCOMMENTS:
XErstellungsdatum und -uhrzeit 92/08/02 18:08:45 von jum
X
XSCCS/s.dlfcn.h:
X
XD 1.3 92/12/27 20:58:32 jum 3 2	00001/00001/00031
XMRs:
XCOMMENTS:
Xwe always have prototypes on RS/6000
X
XD 1.2 92/08/16 17:45:11 jum 2 1	00009/00000/00023
XMRs:
XCOMMENTS:
Xadded dl_info structure to implement initialize and terminate functions
X
XD 1.1 92/08/02 18:08:45 jum 1 0	00023/00000/00000
XMRs:
XCOMMENTS:
XErstellungsdatum und -uhrzeit 92/08/02 18:08:45 von jum
X
SHAR_EOF
$TOUCH -am 0103192093 README &&
chmod 0664 README ||
echo "restore of README failed"
set `wc -c README`;Wc_c=$1
if test "$Wc_c" != "4062"; then
	echo original size 4062, current size $Wc_c
fi
# ============= Makefile ==============
echo "x - extracting Makefile (Text)"
sed 's/^X//' << 'SHAR_EOF' > Makefile &&
X# %W% revision of %E%  %U%
X# This is an unpublished work copyright (c) 1992 Helios Software GmbH
X# 3000 Hannover 1, West Germany
X
XSHELL=/bin/sh
XIPATH=
XDEFS=
XDEBUGFLAGS=-g -DDEBUG
XNODEBUGFLAGS=-O
XCFLAGS=$(IPATH) $(DEFS) $(NODEBUGFLAGS)
XTARGETS=libdl.a
XDEST=/usr/local/lib
XHDRS=dlfcn.h
XSRCS=dlfcn.c
XOBJS=$(SRCS:%.c=%.o)
X
Xall:		$(TARGETS) dlfcn.c
X
X$(TARGETS):	shr.o
X		ar rv $@ $?
X
Xdlfcn.o:	dlfcn.h
X
Xshr.o:		$(OBJS) dl.exp
X		$(CC) -o $@ $(OBJS) -bE:dl.exp -bM:SRE -e _nostart -lld
X
Xlint:
X		lint $(IPATH) $(DEFS) $(SRCS) >lintout
X
Xinfo:
X		sccs info
X
Xclean:
X		rm -f lintout a.out core *.o *-lg *% *~ tags deps%
X
Xclobber:	clean
X		rm -f $(TARGETS) deps
X
Xinstall:	all
X		cp $(TARGETS) $(DEST)
X
Xshar:
X		shar README Makefile dlfcn.h dlfcn.c dl.exp >dl.shar
SHAR_EOF
$TOUCH -am 0103190593 Makefile &&
chmod 0664 Makefile ||
echo "restore of Makefile failed"
set `wc -c Makefile`;Wc_c=$1
if test "$Wc_c" != "745"; then
	echo original size 745, current size $Wc_c
fi
# ============= dlfcn.h ==============
echo "x - extracting dlfcn.h (Text)"
sed 's/^X//' << 'SHAR_EOF' > dlfcn.h &&
X/*
X * @(#)dlfcn.h	1.3 revision of 92/12/27  20:58:32
X * This is an unpublished work copyright (c) 1992 Helios Software GmbH
X * 3000 Hannover 1, Germany
X */
X
X/*
X * Mode flags for the dlopen routine.
X */
X#define RTLD_LAZY	1
X#define RTLD_NOW	2
X
X/*
X * To be able to intialize, a library may provide a dl_info structure
X * that contains functions to be called to initialize and terminate.
X */
Xstruct dl_info {
X	void (*init)(void);
X	void (*fini)(void);
X};
X
X#if __STDC__ || defined(_IBMR2)
Xvoid *dlopen(const char *path, int mode);
Xvoid *dlsym(void *handle, const char *symbol);
Xchar *dlerror(void);
Xint dlclose(void *handle);
X#else
Xvoid *dlopen();
Xvoid *dlsym();
Xchar *dlerror();
Xint dlclose();
X#endif
SHAR_EOF
$TOUCH -am 1227210092 dlfcn.h &&
chmod 0444 dlfcn.h ||
echo "restore of dlfcn.h failed"
set `wc -c dlfcn.h`;Wc_c=$1
if test "$Wc_c" != "696"; then
	echo original size 696, current size $Wc_c
fi
# ============= dlfcn.c ==============
echo "x - extracting dlfcn.c (Text)"
sed 's/^X//' << 'SHAR_EOF' > dlfcn.c &&
X/*
X * @(#)dlfcn.c	1.5 revision of 93/02/14  20:14:17
X * This is an unpublished work copyright (c) 1992 Helios Software GmbH
X * 3000 Hannover 1, Germany
X */
X
X#include <stdio.h>
X#include <errno.h>
X#include <string.h>
X#include <stdlib.h>
X#include <sys/types.h>
X#include <sys/ldr.h>
X#include <a.out.h>
X#include <ldfcn.h>
X#include "dlfcn.h"
X
X/*
X * We simulate dlopen() et al. through a call to load. Because AIX has
X * no call to find an exported symbol we read the loader section of the
X * loaded module and build a list of exported symbols and their virtual
X * address.
X */
X
Xtypedef struct {
X	char		*name;		/* the symbols's name */
X	void		*addr;		/* its relocated virtual address */
X} Export, *ExportPtr;
X
X/*
X * The void * handle returned from dlopen is actually a ModulePtr.
X */
Xtypedef struct Module {
X	struct Module	*next;
X	char		*name;		/* module name for refcounting */
X	int		refCnt;		/* the number of references */
X	void		*entry;		/* entry point from load */
X	struct dl_info	*info;		/* optional init/terminate functions */
X	int		nExports;	/* the number of exports found */
X	ExportPtr	exports;	/* the array of exports */
X} Module, *ModulePtr;
X
X/*
X * We keep a list of all loaded modules to be able to call the fini
X * handlers at atexit() time.
X */
Xstatic ModulePtr modList;
X
X/*
X * The last error from one of the dl* routines is kept in static
X * variables here. Each error is returned only once to the caller.
X */
Xstatic char errbuf[BUFSIZ];
Xstatic int errvalid;
X
Xextern char *strdup(const char *);
Xstatic void caterr(char *);
Xstatic int readExports(ModulePtr);
Xstatic void terminate(void);
Xstatic void *findMain(void);
X
X/* ARGSUSED */
Xvoid *dlopen(const char *path, int mode)
X{
X	register ModulePtr mp;
X	static void *mainModule;
X
X	/*
X	 * Upon the first call register a terminate handler that will
X	 * close all libraries. Also get a reference to the main module
X	 * for use with loadbind.
X	 */
X	if (!mainModule) {
X		if ((mainModule = findMain()) == NULL)
X			return NULL;
X		atexit(terminate);
X	}
X	/*
X	 * Scan the list of modules if have the module already loaded.
X	 */
X	for (mp = modList; mp; mp = mp->next)
X		if (strcmp(mp->name, path) == 0) {
X			mp->refCnt++;
X			return mp;
X		}
X	if ((mp = (ModulePtr)calloc(1, sizeof(*mp))) == NULL) {
X		errvalid++;
X		strcpy(errbuf, "calloc: ");
X		strcat(errbuf, strerror(errno));
X		return NULL;
X	}
X	if ((mp->name = strdup(path)) == NULL) {
X		errvalid++;
X		strcpy(errbuf, "strdup: ");
X		strcat(errbuf, strerror(errno));
X		free(mp);
X		return NULL;
X	}
X	/*
X	 * load should be declared load(const char *...). Thus we
X	 * cast the path to a normal char *. Ugly.
X	 */
X	if ((mp->entry = (void *)load((char *)path, L_NOAUTODEFER, NULL)) == NULL) {
X		free(mp->name);
X		free(mp);
X		errvalid++;
X		strcpy(errbuf, "dlopen: ");
X		strcat(errbuf, path);
X		strcat(errbuf, ": ");
X		/*
X		 * If AIX says the file is not executable, the error
X		 * can be further described by querying the loader about
X		 * the last error.
X		 */
X		if (errno == ENOEXEC) {
X			char *tmp[BUFSIZ/sizeof(char *)];
X			if (loadquery(L_GETMESSAGES, tmp, sizeof(tmp)) == -1)
X				strcpy(errbuf, strerror(errno));
X			else {
X				char **p;
X				for (p = tmp; *p; p++)
X					caterr(*p);
X			}
X		} else
X			strcat(errbuf, strerror(errno));
X		return NULL;
X	}
X	mp->refCnt = 1;
X	mp->next = modList;
X	modList = mp;
X	if (loadbind(0, mainModule, mp->entry) == -1) {
X		dlclose(mp);
X		errvalid++;
X		strcpy(errbuf, "loadbind: ");
X		strcat(errbuf, strerror(errno));
X		return NULL;
X	}
X	if (readExports(mp) == -1) {
X		dlclose(mp);
X		return NULL;
X	}
X	/*
X	 * If there is a dl_info structure, call the init function.
X	 */
X	if (mp->info = (struct dl_info *)dlsym(mp, "dl_info")) {
X		if (mp->info->init)
X			(*mp->info->init)();
X	} else
X		errvalid = 0;
X	return mp;
X}
X
X/*
X * Attempt to decipher an AIX loader error message and append it
X * to our static error message buffer.
X */
Xstatic void caterr(char *s)
X{
X	register char *p = s;
X
X	while (*p >= '0' && *p <= '9')
X		p++;
X	switch(atoi(s)) {
X	case L_ERROR_TOOMANY:
X		strcat(errbuf, "to many errors");
X		break;
X	case L_ERROR_NOLIB:
X		strcat(errbuf, "can't load library");
X		strcat(errbuf, p);
X		break;
X	case L_ERROR_UNDEF:
X		strcat(errbuf, "can't find symbol");
X		strcat(errbuf, p);
X		break;
X	case L_ERROR_RLDBAD:
X		strcat(errbuf, "bad RLD");
X		strcat(errbuf, p);
X		break;
X	case L_ERROR_FORMAT:
X		strcat(errbuf, "bad exec format in");
X		strcat(errbuf, p);
X		break;
X	case L_ERROR_ERRNO:
X		strcat(errbuf, strerror(atoi(++p)));
X		break;
X	default:
X		strcat(errbuf, s);
X		break;
X	}
X}
X
Xvoid *dlsym(void *handle, const char *symbol)
X{
X	register ModulePtr mp = (ModulePtr)handle;
X	register ExportPtr ep;
X	register int i;
X
X	/*
X	 * Could speed up search, but I assume that one assigns
X	 * the result to function pointers anyways.
X	 */
X	for (ep = mp->exports, i = mp->nExports; i; i--, ep++)
X		if (strcmp(ep->name, symbol) == 0)
X			return ep->addr;
X	errvalid++;
X	strcpy(errbuf, "dlsym: undefined symbol ");
X	strcat(errbuf, symbol);
X	return NULL;
X}
X
Xchar *dlerror(void)
X{
X	if (errvalid) {
X		errvalid = 0;
X		return errbuf;
X	}
X	return NULL;
X}
X
Xint dlclose(void *handle)
X{
X	register ModulePtr mp = (ModulePtr)handle;
X	int result;
X	register ModulePtr mp1;
X
X	if (--mp->refCnt > 0)
X		return 0;
X	if (mp->info && mp->info->fini)
X		(*mp->info->fini)();
X	result = unload(mp->entry);
X	if (result == -1) {
X		errvalid++;
X		strcpy(errbuf, strerror(errno));
X	}
X	if (mp->exports) {
X		register ExportPtr ep;
X		register int i;
X		for (ep = mp->exports, i = mp->nExports; i; i--, ep++)
X			if (ep->name)
X				free(ep->name);
X		free(mp->exports);
X	}
X	if (mp == modList)
X		modList = mp->next;
X	else {
X		for (mp1 = modList; mp1; mp1 = mp1->next)
X			if (mp1->next == mp) {
X				mp1->next = mp->next;
X				break;
X			}
X	}
X	free(mp->name);
X	free(mp);
X	return result;
X}
X
Xstatic void terminate(void)
X{
X	while (modList)
X		dlclose(modList);
X}
X
X/*
X * Build the export table from the XCOFF .loader section.
X */
Xstatic int readExports(ModulePtr mp)
X{
X	LDFILE *ldp = NULL;
X	SCNHDR sh;
X	LDHDR *lhp;
X	char *ldbuf;
X	LDSYM *ls;
X	int i;
X	ExportPtr ep;
X
X	if ((ldp = ldopen(mp->name, ldp)) == NULL) {
X		struct ld_info *lp;
X		char *buf;
X		int size = 4*1024;
X		if (errno != ENOENT) {
X			errvalid++;
X			strcpy(errbuf, "readExports: ");
X			strcat(errbuf, strerror(errno));
X			return -1;
X		}
X		/*
X		 * The module might be loaded due to the LIBPATH
X		 * environment variable. Search for the loaded
X		 * module using L_GETINFO.
X		 */
X		if ((buf = malloc(size)) == NULL) {
X			errvalid++;
X			strcpy(errbuf, "readExports: ");
X			strcat(errbuf, strerror(errno));
X			return -1;
X		}
X		while ((i = loadquery(L_GETINFO, buf, size)) == -1 && errno == ENOMEM) {
X			free(buf);
X			size += 4*1024;
X			if ((buf = malloc(size)) == NULL) {
X				errvalid++;
X				strcpy(errbuf, "readExports: ");
X				strcat(errbuf, strerror(errno));
X				return -1;
X			}
X		}
X		if (i == -1) {
X			errvalid++;
X			strcpy(errbuf, "readExports: ");
X			strcat(errbuf, strerror(errno));
X			free(buf);
X			return -1;
X		}
X		/*
X		 * Traverse the list of loaded modules. The entry point
X		 * returned by load() does actually point to the data
X		 * segment origin.
X		 */
X		lp = (struct ld_info *)buf;
X		while (lp) {
X			if (lp->ldinfo_dataorg == mp->entry) {
X				ldp = ldopen(lp->ldinfo_filename, ldp);
X				break;
X			}
X			if (lp->ldinfo_next == 0)
X				lp = NULL;
X			else
X				lp = (struct ld_info *)((char *)lp + lp->ldinfo_next);
X		}
X		free(buf);
X		if (!ldp) {
X			errvalid++;
X			strcpy(errbuf, "readExports: ");
X			strcat(errbuf, strerror(errno));
X			return -1;
X		}
X	}
X	if (TYPE(ldp) != U802TOCMAGIC) {
X		errvalid++;
X		strcpy(errbuf, "readExports: bad magic");
X		while(ldclose(ldp) == FAILURE)
X			;
X		return -1;
X	}
X	if (ldnshread(ldp, _LOADER, &sh) != SUCCESS) {
X		errvalid++;
X		strcpy(errbuf, "readExports: cannot read loader section header");
X		while(ldclose(ldp) == FAILURE)
X			;
X		return -1;
X	}
X	/*
X	 * We read the complete loader section in one chunk, this makes
X	 * finding long symbol names residing in the string table easier.
X	 */
X	if ((ldbuf = (char *)malloc(sh.s_size)) == NULL) {
X		errvalid++;
X		strcpy(errbuf, "readExports: ");
X		strcat(errbuf, strerror(errno));
X		while(ldclose(ldp) == FAILURE)
X			;
X		return -1;
X	}
X	if (FSEEK(ldp, sh.s_scnptr, BEGINNING) != OKFSEEK) {
X		errvalid++;
X		strcpy(errbuf, "readExports: cannot seek to loader section");
X		free(ldbuf);
X		while(ldclose(ldp) == FAILURE)
X			;
X		return -1;
X	}
X	if (FREAD(ldbuf, sh.s_size, 1, ldp) != 1) {
X		errvalid++;
X		strcpy(errbuf, "readExports: cannot read loader section");
X		free(ldbuf);
X		while(ldclose(ldp) == FAILURE)
X			;
X		return -1;
X	}
X	lhp = (LDHDR *)ldbuf;
X	ls = (LDSYM *)(ldbuf+LDHDRSZ);
X	/*
X	 * Count the number of exports to include in our export table.
X	 */
X	for (i = lhp->l_nsyms; i; i--, ls++) {
X		if (!LDR_EXPORT(*ls))
X			continue;
X		mp->nExports++;
X	}
X	if ((mp->exports = (ExportPtr)calloc(mp->nExports, sizeof(*mp->exports))) ==
NULL) {
X		errvalid++;
X		strcpy(errbuf, "readExports: ");
X		strcat(errbuf, strerror(errno));
X		free(ldbuf);
X		while(ldclose(ldp) == FAILURE)
X			;
X		return -1;
X	}
X	/*
X	 * Fill in the export table. All entries are relative to
X	 * the entry point we got from load.
X	 */
X	ep = mp->exports;
X	ls = (LDSYM *)(ldbuf+LDHDRSZ);
X	for (i = lhp->l_nsyms; i; i--, ls++) {
X		char *symname;
X		if (!LDR_EXPORT(*ls))
X			continue;
X		if (ls->l_zeroes == 0)
X			symname = ls->l_offset+lhp->l_stoff+ldbuf;
X		else
X			symname = ls->l_name;
X		ep->name = strdup(symname);
X		ep->addr = (void *)((unsigned long)mp->entry + ls->l_value);
X		ep++;
X	}
X	free(ldbuf);
X	while(ldclose(ldp) == FAILURE)
X		;
X	return 0;
X}
X
X/*
X * Find the main modules entry point. This is used as export pointer
X * for loadbind() to be able to resolve references to the main part.
X */
Xstatic void * findMain(void)
X{
X	struct ld_info *lp;
X	char *buf;
X	int size = 4*1024;
X	int i;
X	void *ret;
X
X	if ((buf = malloc(size)) == NULL) {
X		errvalid++;
X		strcpy(errbuf, "findMain: ");
X		strcat(errbuf, strerror(errno));
X		return NULL;
X	}
X	while ((i = loadquery(L_GETINFO, buf, size)) == -1 && errno == ENOMEM) {
X		free(buf);
X		size += 4*1024;
X		if ((buf = malloc(size)) == NULL) {
X			errvalid++;
X			strcpy(errbuf, "findMain: ");
X			strcat(errbuf, strerror(errno));
X			return NULL;
X		}
X	}
X	if (i == -1) {
X		errvalid++;
X		strcpy(errbuf, "findMain: ");
X		strcat(errbuf, strerror(errno));
X		free(buf);
X		return NULL;
X	}
X	/*
X	 * The first entry is the main module. The entry point
X	 * returned by load() does actually point to the data
X	 * segment origin.
X	 */
X	lp = (struct ld_info *)buf;
X	ret = lp->ldinfo_dataorg;
X	free(buf);
X	return ret;
X}
SHAR_EOF
$TOUCH -am 0214201593 dlfcn.c &&
chmod 0444 dlfcn.c ||
echo "restore of dlfcn.c failed"
set `wc -c dlfcn.c`;Wc_c=$1
if test "$Wc_c" != "10486"; then
	echo original size 10486, current size $Wc_c
fi
# ============= dl.exp ==============
echo "x - extracting dl.exp (Text)"
sed 's/^X//' << 'SHAR_EOF' > dl.exp &&
X#!/usr/local/lib/libdl.a
Xdlopen
Xdlclose
Xdlsym
Xdlerror
SHAR_EOF
$TOUCH -am 0802175192 dl.exp &&
chmod 0664 dl.exp ||
echo "restore of dl.exp failed"
set `wc -c dl.exp`;Wc_c=$1
if test "$Wc_c" != "54"; then
	echo original size 54, current size $Wc_c
fi
exit 0
#endif
