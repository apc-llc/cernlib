*
* $Id: sxyb.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: sxyb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE SXYB (NW,LV,Y,X,SCAL)
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
      DIMENSION  Y (*), X(*)
      DO 1 I = 1,NW
        IF (LV(I)) Y(I) = SCAL*X(I)*Y(I)
    1 CONTINUE
      RETURN
      END
