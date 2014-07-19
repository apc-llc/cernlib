*
* $Id: czdpts.F,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
*
* $Log: czdpts.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDPTS(CHSTR,NTOT,ISTAT)
      CHARACTER*(*) CHSTR
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/czunit.inc"
*
      IF(IDEBXZ.GE.3) THEN
         PRINT *,'CZDPTS put ',NTOT,' characters'
         NPR = MIN(NTOT,80)
         PRINT *,CHSTR(1:NPR)
      ENDIF
 
      WRITE(UNIT=LUNDEC,FMT='(A)',ERR=99) CHSTR(1:NTOT)
      ISTAT = 0
      RETURN
99    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** Error in CZDPTS for unit: ',LUNDEC
      ISTAT = 1
      END
 
