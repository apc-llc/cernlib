*
* $Id: ihsdrl.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihsdrl.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/06 05/01/94  17.53.24  by  O.Couet
*-- Author :
      SUBROUTINE IHSDRL(R1,R2)
************************************************************************
*                                                                      *
*     IHSDRL                                     Date:    06.08.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 13.09.90     *
*                                                         10.01.92     *
*                                                         06.11.92     *
*     Function: Find visible parts of line (draw line)                 *
*                                                                      *
*     Input: R1(3)  - 1-st point of the line                           *
*            R2(3)  - 2-nd point of the line                           *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
      REAL              R1(3),R2(3)
*-
      X1     = TN(1,1)*R1(1)+TN(2,1)*R1(2)+TN(3,1)*R1(3)+TN(4,1)
      X2     = TN(1,1)*R2(1)+TN(2,1)*R2(2)+TN(3,1)*R2(3)+TN(4,1)
      Y1     = TN(1,2)*R1(1)+TN(2,2)*R1(2)+TN(3,2)*R1(3)+TN(4,2)
      Y2     = TN(1,2)*R2(1)+TN(2,2)*R2(2)+TN(3,2)*R2(3)+TN(4,2)
      Z1     = TN(1,3)*R1(1)+TN(2,3)*R1(2)+TN(3,3)*R1(3)+TN(4,3)
      Z2     = TN(1,3)*R2(1)+TN(2,3)*R2(2)+TN(3,3)*R2(3)+TN(4,3)
      IFBACK = 0
      IF (X1 .LT. X2)                   GOTO 10
      IFBACK = 1
      WW     = X1
      X1     = X2
      X2     = WW
      WW     = Y1
      Y1     = Y2
      Y2     = WW
      WW     = Z1
      Z1     = Z2
      Z2     = WW
   10 NT     = 0
      I1     = (X1-X0)/DX + 1.5
      I2     = (X2-X0)/DX + 1.5
      X1     = X0 + (I1-1)*DX
      X2     = X0 + (I2-1)*DX
      IF (I1 .EQ. I2)                   GOTO 140
*
**          F I N D   V I S I B L E   P A R T S   O F   T H E   L I N E
*
      DI     = I2 - I1
      DY     = (Y2-Y1) / DI
      DT     = 1. / DI
      IV     =-1
      DO 120 I=I1,I2-1
         YY1  = Y1 + DY*(I-I1)
         YY2  = YY1 + DY
         YY1U = YY1 - U(1,I)
         YY1D = YY1 - D(1,I)
         YY2U = YY2 - U(2,I)
         YY2D = YY2 - D(2,I)
         TT   = DT*(I-I1)
*           A N A L I S E   L E F T   S I D E
         ICASE1 = 1
         IF (YY1U .GT. 0.) ICASE1 = 0
         IF (YY1D .LT. 0.) ICASE1 = 2
         GOTO (20 ,30 ,20 ), ICASE1+1
   20    IF (IV .GT. 0) GOTO 40
         IV      = +1
         NT      = NT + 1
         T(1,NT) = TT
         GOTO 40
   30    IF (IV .LT. 0) GOTO 40
         IV      = -1
         T(2,NT) = TT
         GOTO 40
*           A N A L I S E   R I G H T   S I D E
   40    ICASE2  = 1
         IF (YY2U .GT. 0.) ICASE2 = 0
         IF (YY2D .LT. 0.) ICASE2 = 2
         ICASE = ICASE1*3 + ICASE2
         GOTO (110,50 ,60 , 70 ,110,80 , 90 ,100,110), ICASE+1
   50    IV      = -1
         T(2,NT) = TT + DT*(YY1U/(YY1U-YY2U))
         GOTO 110
   60    T(2,NT) = TT + DT*(YY1U/(YY1U-YY2U))
         NT      = NT + 1
         T(1,NT) = TT + DT*(YY1D/(YY1D-YY2D))
         GOTO 110
   70    IV      = +1
         NT      = NT + 1
         T(1,NT) = TT + DT*(YY1U/(YY1U-YY2U))
         GOTO 110
   80    IV      = +1
         NT      = NT + 1
         T(1,NT) = TT + DT*(YY1D/(YY1D-YY2D))
         GOTO 110
   90    T(2,NT) = TT + DT*(YY1D/(YY1D-YY2D))
         NT      = NT + 1
         T(1,NT) = TT + DT*(YY1U/(YY1U-YY2U))
         GOTO 110
  100    IV      = -1
         T(2,NT) = TT + DT*(YY1D/(YY1D-YY2D))
         GOTO 110
  110    IF (NT+1 .GE. NTMAX) GOTO 130
  120 CONTINUE
  130 IF (IV .GT. 0)                    T(2,NT) = 1.
      GOTO 180
*
**          V E R T I C A L   L I N E
*
  140 NT     = 1
      T(1,1) = 0.
      T(2,1) = 1.
      IF (Y2 .GT. Y1)                   GOTO 150
      IF (Y2 .EQ. Y1)                   GOTO 200
      IFBACK = 1 - IFBACK
      YY     = Y1
      Y1     = Y2
      Y2     = YY
  150 UU     = U(1,I1)
      DD     = D(1,I1)
      IF (I1 .EQ. 1)                    GOTO 160
      IF (UU .LT. U(2,I1-1))            UU = U(2,I1-1)
      IF (DD .GT. D(2,I1-1))            DD = D(2,I1-1)
*           F I N D   V I S I B L E   P A R T   O F   L I N E
  160 IF (Y1.GE.UU .OR. Y2.LE.DD)       GOTO 180
      IF (Y1.GE.DD .AND. Y2.LE.UU)      GOTO 200
      NT     = 0
      IF (DD .LE. Y1)                   GOTO 170
      NT     = NT + 1
      T(1,NT)= 0.
      T(2,NT)= (DD-Y1) / (Y2-Y1)
  170 IF (UU .GE. Y2)                   GOTO 180
      NT     = NT + 1
      T(1,NT)= (UU-Y1) / (Y2-Y1)
      T(2,NT)= 1.
*
  180 IF (IFBACK .EQ. 0)                RETURN
      IF (NT .EQ. 0)                    RETURN
      DO 190 I=1,NT
         T(1,I) = 1. - T(1,I)
         T(2,I) = 1. - T(2,I)
  190 CONTINUE
      RETURN
*
  200 NT     = 0
      END
