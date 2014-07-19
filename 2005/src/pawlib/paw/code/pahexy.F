*
* $Id: pahexy.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahexy.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.15/01 14/09/92  16.46.56  by  O.Couet
*-- Author :    O.Couet   14/09/92
      SUBROUTINE PAHEXY (r,zslp,Z,X,Y)
C--------------------
C  called by: PAFEYN
C  helix eq.
C--------------------
*
      IF (ABS(ZSLP).LT.1.E-9) RETURN
      S =     Z/ZSLP
      ALPHA = S/R/2.
      IF (ABS(ALPHA).LT.1.E-9) THEN
        RAD = S
      ELSE IF (ABS(ALPHA).GT.1.E6) THEN
        RETURN
      ELSE
        RAD = 2.*R* SIN(ALPHA)
      ENDIF
      X = -RAD*SIN(ALPHA)
      Y =  RAD*COS(ALPHA)
*
      END
