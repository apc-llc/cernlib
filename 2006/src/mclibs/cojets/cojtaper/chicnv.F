*
* $Id: chicnv.F,v 1.1.1.1 1996/01/11 14:15:04 mclareni Exp $
*
* $Log: chicnv.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:04  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE CHICNV(CHAN,IL,N)
C     ****************************
C-- CONVERTS IZEVEL ASCII CODE INTEGER INTO CHARACTER
#include "cojets/zevel.inc"
      CHARACTER*1 CHAN(N)
      CHARACTER*1 CHAR
      DO 1 L=1,N
      CHAN(L)=CHAR(IZEVEL(IL))
      IL=IL+1
1     CONTINUE
      RETURN
      END
