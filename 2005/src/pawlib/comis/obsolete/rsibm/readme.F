*
* $Id: readme.F,v 1.1.1.1 1996/02/26 17:16:46 mclareni Exp $
*
* $Log: readme.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:46  mclareni
* Comis
*
*
#include "sys/CERNLIB_machine.h"
#include "_comis/pilot.h"
#if defined(CERNLIB_NEVER)
/*CMZ :  1.18/02 13/04/94  17.43.28  by  Fons Rademakers*/
/*-- Author :    Vladimir Berezhnoi   01/04/94*/
Copyright (c) 1992,1993, Jens-Uwe Mager, Helios Software GmbH
Not derived from licensed software.

Permission is granted to freely use, copy, modify, and redistribute
this software, provided that no attempt is made to gain profit from it,
the author is not construed to be liable for any results of using the
software, alterations are clearly marked as such, and this notice is
not modified.

libdl.a
-------

This is an emulation library to emulate the SunOS/System V.4 functions
to access the runtime linker. The functions are emulated by using the
AIX load() function and by reading the .loader section of the loaded
module to find the exports. The to be loaded module should be linked as
follows:

	cc -o module.so -bM:SRE -bE:module.exp -e _nostart $(OBJS)

The module export file contains the symbols to be exported. Because
this library uses the loader section, the final module.so file can be
stripped.

Usage
-----

void *dlopen(const char *path, int mode);

This routine loads the module pointed to by path and reads its export
table. If the path does not contain a '/' character, dlopen will search
for the module using the LIBPATH environment variable. It returns an
opaque handle to the module or NULL on error. The flags parameter is
currently ignored.


void *dlsym(void *handle, const char *symbol);

This routine searches for the symbol in the module referred to by
handle and returns its address. If the symbol could not be found, the
function returns NULL. The return value must be casted to a proper
function pointer before it can be used. SunOS/System V.4 allow handle
to be a NULL pointer to refer to the module the call is made from, this
is not implemented.

int dlclose(void *handle);

This routine unloads the module referred to by the handle and disposes
of any local storage. this function returns -1 on failure.

char *dlerror(void);

This routine can be used to retrieve a text message describing the most
recent error that occured on on of the above routines. This function
returns NULL if there is not error information.

Initialization and termination handlers
---------------------------------------

The emulation provides for an initialization and a termination
handler.  The dlfcn.h file contains a structure declaration named
dl_info with following members:

	void (*init)(void);
	void (*fini)(void);

The init function is called upon first referencing the library. The
fini function is called at dlclose() time or when the process exits.
The module should declare a variable named dl_info that contains this
structure which must be exported.  These functions correspond to the
documented _init() and _fini() functions of SunOS 4.x, but these are
appearently not implemented in SunOS.  When using SunOS 5.0, these
correspond to #pragma init and #pragma fini respectively.

Jens-Uwe Mager

HELIOS Software GmbH
Lavesstr. 80
3000 Hannover 1
Germany

Phone:		+49 511 3681093
FAX:		+49 511 3681095
AppleLink:	ger.xse0082	Attn: Jens-Uwe Mager
uucp:		jum@helios.de	or heliosd!jum

Revision history:
-----------------

SCCS/s.dlfcn.c:

D 1.4 93/01/03 19:13:56 jum 4 3	00061/00005/00403
MRs:
COMMENTS:
to allow calling symbols in the main module call load with L_NOAUTODEFER and
do a loadbind later with the main module.

D 1.3 92/12/27 20:59:55 jum 3 2	00066/00008/00342
MRs:
COMMENTS:
added search by L_GETINFO if module got loaded by LIBPATH

D 1.2 92/08/16 17:45:43 jum 2 1	00074/00006/00276
MRs:
COMMENTS:
implemented initialize and terminate functions, added reference counting to
avoid multiple loads of the same library

D 1.1 92/08/02 18:08:45 jum 1 0	00282/00000/00000
MRs:
COMMENTS:
Erstellungsdatum und -uhrzeit 92/08/02 18:08:45 von jum

SCCS/s.dlfcn.h:

D 1.3 92/12/27 20:58:32 jum 3 2	00001/00001/00031
MRs:
COMMENTS:
we always have prototypes on RS/6000

D 1.2 92/08/16 17:45:11 jum 2 1	00009/00000/00023
MRs:
COMMENTS:
added dl_info structure to implement initialize and terminate functions

D 1.1 92/08/02 18:08:45 jum 1 0	00023/00000/00000
MRs:
COMMENTS:
Erstellungsdatum und -uhrzeit 92/08/02 18:08:45 von jum

#endif
