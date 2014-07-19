*
* $Id: patchy.F,v 1.1.1.1 1996/02/15 17:51:00 mclareni Exp $
*
* $Log: patchy.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:00  mclareni
* Kernlib
*
*
*    Patch boot-strap
#include "kernapo/pilot.h"
#if defined(CERNLIB_P5BOOT)
#ifndef CERNLIB_P5_CC
#define CERNLIB_P5_CC
#endif
#ifndef CERNLIB_P5_CI
#define CERNLIB_P5_CI
#endif
#ifndef CERNLIB_APOGS_IANDETC
#define CERNLIB_APOGS_IANDETC
#endif
#ifndef CERNLIB_APOGS_ISHFT
#define CERNLIB_APOGS_ISHFT
#endif
#ifndef CERNLIB_APOGS_JBIT
#define CERNLIB_APOGS_JBIT
#endif
#ifndef CERNLIB_APOGS_JBYT
#define CERNLIB_APOGS_JBYT
#endif
#ifndef CERNLIB_APOGS_LENOCC
#define CERNLIB_APOGS_LENOCC
#endif
#ifndef CERNLIB_APOGS_LOCF
#define CERNLIB_APOGS_LOCF
#endif
#ifndef CERNLIB_APOGS_PKBYT
#define CERNLIB_APOGS_PKBYT
#endif
#ifndef CERNLIB_APOGS_SBIT0
#define CERNLIB_APOGS_SBIT0
#endif
#ifndef CERNLIB_APOGS_SBIT1
#define CERNLIB_APOGS_SBIT1
#endif
#ifndef CERNLIB_APOGS_SBYT
#define CERNLIB_APOGS_SBYT
#endif
#ifndef CERNLIB_APOGS_UCTOH
#define CERNLIB_APOGS_UCTOH
#endif
#ifndef CERNLIB_APOGS_UPKBYT
#define CERNLIB_APOGS_UPKBYT
#endif
#ifndef CERNLIB_APOSYS_DATIME
#define CERNLIB_APOSYS_DATIME
#endif
#ifndef CERNLIB_APOSYS_GETARG
#define CERNLIB_APOSYS_GETARG
#endif
#ifndef CERNLIB_APOSYS_IARGC
#define CERNLIB_APOSYS_IARGC
#endif
#ifndef CERNLIB_APOSYS_INTRAC
#define CERNLIB_APOSYS_INTRAC
#endif
#ifndef CERNLIB_APOSYS_SIGNALF
#define CERNLIB_APOSYS_SIGNALF
#endif
#ifndef CERNLIB_APOSYS_TRACEQ
#define CERNLIB_APOSYS_TRACEQ
#endif
#endif
