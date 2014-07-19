*
* $Id: ihaxiv.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: ihaxiv.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 08/04/93  16.29.10  by  O.Couet
*-- Author :
      SUBROUTINE IHAXIV(RMIN,RMAX,ANG,AV,IX1,IX2,IY1,IY2,IZ1,IZ2)
************************************************************************
*                                                                      *
*     IHAXIV                                     Date:    20.09.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Define axis vertecies                                  *
*                                                                      *
*     Input: RMIN(3) - min-                                            *
*            RMAX(3) -     max scope                                   *
*            ANG     - angle between X and Y axis                      *
*                                                                      *
*     Output: AV(3,8) - axis vertixes                                  *
*             IX1     - 1st point of X-axis                            *
*             IX2     - 2nd point of X-axis                            *
*             IY1     - 1st point of Y-axis                            *
*             IY2     - 2nd point of Y-axis                            *
*             IZ1     - 1st point of Z-axis                            *
*             IZ2     - 2nd point of Z-axis                            *
*                                                                      *
*                  8                        6                          *
*                 / \                      /|\                         *
*              5 /   \ 7                5 / | \ 7                      *
*               |\   /|                  |  |  |                       *
*   THETA < 90  | \6/ |     THETA > 90   | /2\ |                       *
*   (Top view)  |  |  |   (Bottom view)  |/   \|                       *
*              1 \ | /3                 1 \   /3                       *
*                 \|/                      \ /                         *
*                  2                        4                          *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
      REAL              RMIN(3),RMAX(3),AV(3,8),P(2,4)
      DOUBLE PRECISION  RAD,COSA,SINA
*-
      RAD    = ATAN(1.D0)*4./180.
      SINA   = SIN(ANG*RAD)
      COSA   = COS(ANG*RAD)
      P(1,1) = RMIN(1)
      P(2,1) = RMIN(2)
      P(1,2) = RMAX(1)
      P(2,2) = RMIN(2)
      P(1,3) = RMAX(1)
      P(2,3) = RMAX(2)
      P(1,4) = RMIN(1)
      P(2,4) = RMAX(2)
*           F I N D   T H E   M O S T   L E F T   P O I N T
      I1     = 1
      IF (TN(1,1) .LT. 0.)                      I1 = 2
      IF (TN(1,1)*COSA+TN(2,1)*SINA .LT. 0.)    I1 = 5 - I1
*          S E T   O T H E R   P O I N T S
      I2     = MOD(I1,4) + 1
      I3     = MOD(I2,4) + 1
      I4     = MOD(I3,4) + 1
*
**        S E T   A X I S   V E R T I X E S
*
      AV(1,1) = P(1,I1)
      AV(2,1) = P(2,I1)
      AV(1,2) = P(1,I2)
      AV(2,2) = P(2,I2)
      AV(1,3) = P(1,I3)
      AV(2,3) = P(2,I3)
      AV(1,4) = P(1,I4)
      AV(2,4) = P(2,I4)
      DO 100 I=1,4
        AV(3,I)   = RMIN(3)
        AV(1,I+4) = AV(1,I)
        AV(2,I+4) = AV(2,I)
        AV(3,I+4) = RMAX(3)
  100 CONTINUE
*
**          S E T   A X I S
*
      IF (AV(1,1) .EQ. AV(1,2))         IX = 2
      IF (AV(2,1) .EQ. AV(2,2))         IX = 1
      IY     = 3 - IX
*           X - A X I S
      IX1    = IX
      IF (AV(1,IX) .GT. AV(1,IX+1))     IX1 = IX + 1
      IX2    = IX*2 - IX1 + 1
*           Y - A X I S
      IY1    = IY
      IF (AV(2,IY) .GT. AV(2,IY+1))     IY1 = IY + 1
      IY2    = IY*2 - IY1 + 1
*           Z - A X I S
      IZ1    = 1
      IZ2    = 5
*
      IF (TN(3,3) .GE. 0.)              GOTO 999
      K      = (IX1-1)*3 + IX2
*           11  12  13  21  22  23  31  32  33
      GOTO(999,212,999,221,999,223,999,232,999),K
  212 IX1    = 4
      IX2    = 3
      GOTO 300
  221 IX1    = 3
      IX2    = 4
      GOTO 300
  223 IX1    = 1
      IX2    = 4
      GOTO 300
  232 IX1    = 4
      IX2    = 1
      GOTO 300
*
  300 K      = (IY1-1)*3 + IY2
*           11  12  13  21  22  23  31  32  33
      GOTO(999,312,999,321,999,323,999,332,999),K
  312 IY1    = 4
      IY2    = 3
      GOTO 999
  321 IY1    = 3
      IY2    = 4
      GOTO 999
  323 IY1    = 1
      IY2    = 4
      GOTO 999
  332 IY1    = 4
      IY2    = 1
      GOTO 999
*
  999 END
