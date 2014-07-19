*
* $Id: ztrap.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: ztrap.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
#if defined(CERNLIB_SINGLE)||defined(CERNLIB_DOUBLE)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_BESM6)
      SUBROUTINE ZTRAP(IZ,IUSER)
C
C     ******************************************************************
C     *                                                                *
C     *       STORE ADDRESS OF RECOVERY ROUTINE                        *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
#if defined(CERNLIB_CDC)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_UNIX)
      EXTERNAL IUSER
#endif
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
#if (defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_UNIX))
      IZ(JZ+19) = IADDR(IUSER)
#endif
#if defined(CERNLIB_CDC)
      IZ(JZ+19) = LOCF(IUSER)
#endif
#if defined(CERNLIB_IBM)
      IZ(JZ+19) = IUSER
#endif
#if defined(CERNLIB_NORD)
      IZ(JZ+19) = IUSER
#endif
#if defined(CERNLIB_CRAY)
      IZ(JZ+19) = LOC(IUSER)
#endif
#if defined(CERNLIB_UNIVAC)
      IZ(JZ+19) = LOCF(IUSER)
#endif
#if defined(CERNLIB_VAX)
      IZ(JZ+19) = %LOC(IUSER)
#endif
#if defined(CERNLIB_BESM6)
      IZ(JZ+19) = LOCF(IUSER)
#endif
#if defined(CERNLIB_UNIX)
      IZ(JZ+19) = LOCB(IUSER)
#endif
      END
#endif
