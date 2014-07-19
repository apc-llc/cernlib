*
* $Id: czdgtc.F,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
*
* $Log: czdgtc.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDGTC(NTOT,ISTAT)
*
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/czunit.inc"
*
      IF(IDEBXZ.GE.3) PRINT *,'CZDGTS. get ',NTOT,' characters'
      CHBUF = ' '
      READ( UNIT=LUNDEC, FMT='(A)', END=99, ERR=98 ) CHBUF(1:NTOT)
      ISTAT = 0
      NPR   = MIN(NTOT,80)
      IF(IDEBXZ.GE.3) PRINT *,CHBUF(1:NPR)
      RETURN
98    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZDGTC read error on unit: ',LUNDEC
      ISTAT = 1
      RETURN
99    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZDGTC EOF on unit: ', LUNDEC
      ISTAT = 1
      END
