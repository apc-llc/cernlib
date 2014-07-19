*
* $Id: chmove.F,v 1.1.1.1 1996/04/01 15:03:21 mclareni Exp $
*
* $Log: chmove.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:21  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
#if !defined(CERNLIB_CDC)
      SUBROUTINE CHMOVE(SOURCE,IS,TARGET,IT,NCHAR)
C     Moves NCHAR characters from position IS in SOURCE
C                              to position IT in TARGET
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_UNIX))
C     FORTRAN 4 version    Nov. 1976,   F.James/CERN
      LOGICAL*1 SOURCE(10),TARGET(10)
      IF(NCHAR.LE.0) RETURN
      DO 100 I= 1, NCHAR
  100 TARGET(IT+I-1) = SOURCE(IS+I-1)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)
C     FORTRAN 77 version, May 1983, T.Lindelof/CERN
      CHARACTER*(*) SOURCE,TARGET
      DO 100,I=0,NCHAR-1
  100 TARGET(IT+I:IT+I) = SOURCE(IS+I:IS+1)
#endif
      RETURN
      END
#endif
