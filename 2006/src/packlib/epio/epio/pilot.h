*
* $Id: pilot.h,v 1.4 1998/09/25 09:25:42 mclareni Exp $
*
* $Log: pilot.h,v $
* Revision 1.4  1998/09/25 09:25:42  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.3  1997/09/02 12:45:55  mclareni
* WINNT needs byte swapping
*
* Revision 1.2  1996/09/20 14:07:07  cernlib
* for LINUX select STF77VX
*
* Revision 1.1.1.1  1996/03/08 15:21:47  mclareni
* Epio
*
*
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_VAXULX)
#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_NORD)
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_DECS))&&(!(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)))&&(!defined(CERNLIB_WINNT))
#ifndef CERNLIB_STF77
#define CERNLIB_STF77
#endif
#endif
#if defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
#ifndef CERNLIB_STF77VX
#define CERNLIB_STF77VX
#endif
#endif
