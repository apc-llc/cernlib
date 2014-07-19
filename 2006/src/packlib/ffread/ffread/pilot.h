*
* $Id: pilot.h,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: pilot.h,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#ifndef CERNLIB_UNIX
#define CERNLIB_UNIX
#endif

#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif

#if defined(CERNLIB_UNIX)
#ifndef CERNLIB_SAVE
#define CERNLIB_SAVE
#endif
#endif

#ifndef CERNLIB_UPLOW
#define CERNLIB_UPLOW
#endif

#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)
#ifndef CERNLIB_TABS
#define CERNLIB_TABS
#endif
#endif

#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_CDC))&&(!defined(CERNLIB_DECS))
#ifndef CERNLIB_LUNF77
#define CERNLIB_LUNF77
#endif
#endif
