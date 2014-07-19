*
* $Id: pilot.h,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: pilot.h,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
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

#if (!defined(CERNLIB_QXNO_SC))&&(!defined(CERNLIB_QXCAPT))
#ifndef CERNLIB_QX_SC
#define CERNLIB_QX_SC
#endif
#endif
