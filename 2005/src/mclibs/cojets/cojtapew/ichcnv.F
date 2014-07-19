*
* $Id: ichcnv.F,v 1.1.1.1 1996/01/11 14:15:02 mclareni Exp $
*
* $Log: ichcnv.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:02  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ICHCNV(CHAN,IL,N)
C     ****************************
C-- CONVERTS CHARACTER INTO ASCII CODE INTEGER, PUTS IT INTO IZEVEL
#include "cojets/zevel.inc"
      CHARACTER*1 CHAN(N)
      DO 1 L=1,N
      IZEVEL(IL)=ICHAR(CHAN(L))
      IL=IL+1
1     CONTINUE
      RETURN
      END
