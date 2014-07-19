*
* $Id: aintgb.F,v 1.1.1.1 1996/03/21 17:19:55 mclareni Exp $
*
* $Log: aintgb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:55  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE AINTGB (N,X,X2B,X1B,LV,NB)
C
C  ************************************************************
C  *                                                          *
C  * Author: M.Roethlisberger/IBM                             *
C  *                                                          *
C  * Date:  6 Nov 1990,  V1.01,  Creation of the module       *
C  *                                                          *
C  ************************************************************
C
      LOGICAL    LV(*)
#include "defsp.inc"
     +           X (*), X2B(*), X1B(*)
      NB = 0
      DO 1 I = 1,N
        LV(I) = X(I).LT.X2B(I) .AND. X(I).GT.X1B(I)
        IF (LV(I)) NB = NB + 1
    1 CONTINUE
      RETURN
      END
