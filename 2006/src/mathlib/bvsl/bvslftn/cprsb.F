*
* $Id: cprsb.F,v 1.1.1.1 1996/03/21 17:19:55 mclareni Exp $
*
* $Log: cprsb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:55  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
C --------------- GATHER/SCATTER OPERATIONS -------------------
      SUBROUTINE CPRSB (NW,X,LV)
C
C  ************************************************************
C  *                                                          *
C  * Author: M.Roethlisberger/IBM                             *
C  *                                                          *
C  * Date:  6 Nov 1990,  V1.01,  Creation of the module       *
C  *                                                          *
C  ************************************************************
C
      LOGICAL    LV(NW)
      DIMENSION  X (NW)
 
      J = 0
      DO 1 I = 1,NW
        IF (LV(I)) THEN
          J = J + 1
          X(J) = X(I)
        ENDIF
   1  CONTINUE
      RETURN
      END
