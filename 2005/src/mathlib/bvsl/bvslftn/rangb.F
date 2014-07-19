*
* $Id: rangb.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: rangb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.24  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE RANGB (NW,V,HIGH,LOW,LV,NFND)
C
C  ************************************************************
C  *                                                          *
C  * Author: M.Roethlisberger/IBM                             *
C  *                                                          *
C  * Date:  6 Nov 1990,  V1.01,  Creation of the module       *
C  *                                                          *
C  ************************************************************
C
      DIMENSION  V (NW)
      LOGICAL    LV(NW)
      REAL       LOW
      DO 1 I = 1,NW
        LV(I) = V(I).GE.LOW.AND.V(I).LE.HIGH
    1 CONTINUE
 
      NFND = 0
      DO 2 I = 1,NW
        IF(LV(I)) NFND = NFND+1
    2 CONTINUE
      RETURN
      END
