*
* $Id: orb.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: orb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE ORB (NW,LV1,LV2,LV3,N)
C
C  ************************************************************
C  *                                                          *
C  * Author: M.Roethlisberger/IBM                             *
C  *                                                          *
C  * Date:  6 Nov 1990,  V1.01,  Creation of the module       *
C  *                                                          *
C  ************************************************************
C
      LOGICAL  LV1(NW), LV2(NW), LV3(NW)
 
      N = 0
      DO 1 I = 1,NW
        LV3(I) = LV1(I).OR.LV2(I)
        IF (LV3(I))  N = N + 1
    1 CONTINUE
      RETURN
      END
