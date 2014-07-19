*
* $Id: eisrs1.F,v 1.10 2001/10/08 14:03:14 mclareni Exp $
*
* $Log: eisrs1.F,v $
* Revision 1.10  2001/10/08 14:03:14  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_NOCERN)
      SUBROUTINE EISRS1(NM,N,AR,WR,ZR,IERR,WORK)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC MATRIX
C     FROM CERN PROGRAM LIBRARY
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER NM,N,IERR
      REAL AR(NM,NM),WR(N),ZR(NM,NM),WORK(1)
      CALL TRED2(NM,N,AR,WR,WORK,ZR)
      CALL TQL2(NM,N,WR,WORK,ZR,IERR)
      RETURN
      END
#endif
