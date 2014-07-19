*
* $Id: pawfil.inc,v 1.3 1996/09/13 15:35:19 couet Exp $
*
* $Log: pawfil.inc,v $
* Revision 1.3  1996/09/13 15:35:19  couet
* - OLD Qp remove
*
* Revision 1.2  1996/03/01 16:49:58  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWFIL_INC
#define CERNLIB_PAW_PAWFIL_INC
*
* pawfil.inc
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
#include "paw/pawfil1.inc"
#endif
#if defined(CERNLIB_IBMMVS)
#include "paw/pawfil2.inc"
#endif
#if defined(CERNLIB_VAX)
#include "paw/pawfil3.inc"
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)||defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_WINNT))
#include "paw/pawfil4.inc"
#endif
#if defined(CERNLIB_MSDOS)
#include "paw/pawfil5.inc"
#endif
#if defined(CERNLIB_WINNT)
#include "paw/pawfil6.inc"
#endif

#endif
