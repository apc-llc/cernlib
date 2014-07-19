*
* $Id: czdgts.F,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
*
* $Log: czdgts.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDGTS(CHSTR,NTOT,ISTAT)
*
      CHARACTER*(*) CHSTR
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/czunit.inc"
*
      IF(IDEBXZ.GE.3) PRINT *,'CZDGTS. get ',NTOT,' characters'
      CHSTR = ' '
      READ( UNIT=LUNDEC, FMT='(A)', END=99, ERR=98 ) CHSTR(1:NTOT)
      NPR   = MIN(NTOT,80)
      IF(IDEBXZ.GE.3) PRINT *,CHSTR(1:NPR)
      ISTAT = 0
      RETURN
98    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZDGTS read error on unit: ',LUNDEC
      ISTAT = 1
      RETURN
99    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZDGTS EOF on unit: ', LUNDEC
      ISTAT = 1
      END
