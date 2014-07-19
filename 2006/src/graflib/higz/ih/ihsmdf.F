*
* $Id: ihsmdf.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihsmdf.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/00 19/02/92  10.16.08  by  O.Couet
*-- Author :
      SUBROUTINE IHSMDF(R1,R2)
************************************************************************
*                                                                      *
*     IHSMDF                                     Date:    07.08.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Modify SCREEN                                          *
*                                                                      *
*     Input: R1(3) - 1-st point of the line                            *
*            R2(3) - 2-nd point of the line                            *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
      REAL              R1(3),R2(3)
*-
      X1     = TN(1,1)*R1(1)+TN(2,1)*R1(2)+TN(3,1)*R1(3)+TN(4,1)
      X2     = TN(1,1)*R2(1)+TN(2,1)*R2(2)+TN(3,1)*R2(3)+TN(4,1)
      Y1     = TN(1,2)*R1(1)+TN(2,2)*R1(2)+TN(3,2)*R1(3)+TN(4,2)
      Y2     = TN(1,2)*R2(1)+TN(2,2)*R2(2)+TN(3,2)*R2(3)+TN(4,2)
      IF (X1 .LT. X2)                   GOTO 100
      WW     = X1
      X1     = X2
      X2     = WW
      WW     = Y1
      Y1     = Y2
      Y2     = WW
  100 I1     = (X1-X0)/DX + 1.5
      I2     = (X2-X0)/DX + 1.5
      IF (I1 .EQ. I2)                   GOTO 999
*
**          M O D I F Y   B O U N D A R I E S   OF THE SCREEN
*
      DY     = (Y2-Y1) / (I2-I1)
      DO 200 I=I1,I2-1
        YY1    = Y1 + DY*(I-I1)
        YY2    = YY1 + DY
        IF (D(1,I) .GT. YY1)            D(1,I) = YY1
        IF (D(2,I) .GT. YY2)            D(2,I) = YY2
        IF (U(1,I) .LT. YY1)            U(1,I) = YY1
        IF (U(2,I) .LT. YY2)            U(2,I) = YY2
  200   CONTINUE
*
  999 RETURN
      END
