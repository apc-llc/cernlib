*
* $Id: czdptc.F,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
*
* $Log: czdptc.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDPTC(NTOT,ISTAT)
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
*
      WRITE(UNIT=LUNDEC,FMT='(A)',ERR=99) CHBUF(1:NTOT)
      ISTAT = 0
      RETURN
99    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** Error in CZDPTC for unit: ',LUNDEC
      ISTAT = 1
      END
 
