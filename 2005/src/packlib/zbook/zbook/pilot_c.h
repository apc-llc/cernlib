/*
* $Id: pilot_c.h,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: pilot_c.h,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*/

#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif

#ifdef CERNLIB_APOLLO
#ifndef CERNLIB_QXNO_SC
#define CERNLIB_QXNO_SC
#endif
#endif

#ifdef CERNLIB_CRAY
#ifndef CERNLIB_QXCAPT
#define CERNLIB_QXCAPT
#endif
#endif

#if (!defined(CERNLIB_QXNO_SC))&&(!defined(CERNLIB_QXCAPT))
#ifndef CERNLIB_QX_SC
#define CERNLIB_QX_SC
#endif
#endif
