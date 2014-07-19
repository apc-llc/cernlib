*
* $Id: scttb.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: scttb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE SCTTB (NW,XSCAT,LV,X)
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
      DIMENSION  X (NW) , XSCAT(NW)
 
      J = 0
      DO 1 I = 1,NW
        IF (LV(I)) THEN
          J = J + 1
          XSCAT(I) = X(J)
        ENDIF
   1  CONTINUE
      RETURN
      END
