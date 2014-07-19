*
* $Id: dcopyb.F,v 1.1.1.1 1996/03/21 17:19:55 mclareni Exp $
*
* $Log: dcopyb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:55  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE DCOPYB (NW,LV,Y,X)
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
     +           X (*), Y(*)
      DO 1 I = 1,NW
        IF (LV(I)) Y(I) = X(I)
    1 CONTINUE
      RETURN
      END
