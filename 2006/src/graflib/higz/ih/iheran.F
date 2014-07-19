*
* $Id: iheran.F,v 1.1.1.1 1996/02/14 13:10:49 mclareni Exp $
*
* $Log: iheran.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:49  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 14/01/92  14.01.24  by  O.Couet
*-- Author :
      SUBROUTINE IHERAN(P1,P2,F1,F2,FMIN,FMAX,KPP,PP)
************************************************************************
*                                                                      *
*     IHERAN                                     Date:    20.03.90     *
*     Author: E. Chernyaev (IHEP)                Revised: 13.09.90     *
*                                                                      *
*     Function: Find part of edge where function defined on this edge  *
*               has value from FMIN to FMAX                            *
*                                                                      *
*     Input: P1(3) - 1st point                                         *
*            P2(3) - 2nd point                                         *
*            F1    - function value at 1st point                       *
*            F2    - function value at 2nd point                       *
*            FMIN  - min value of layer                                *
*            FMAX  - max value of layer                                *
*                                                                      *
*     Output: KPP - current number of point                            *
*             PP(3,*) - coordinates of new face                        *
*                                                                      *
************************************************************************
      REAL      P1(3),P2(3),PP(3,*)
*-
      K1    = 0
      IF (F1 .LT. FMIN)         K1 =-2
      IF (F1 .EQ. FMIN)         K1 =-1
      IF (F1 .EQ. FMAX)         K1 =+1
      IF (F1 .GT. FMAX)         K1 =+2
      K2    = 0
      IF (F2 .LT. FMIN)         K2 =-2
      IF (F2 .EQ. FMIN)         K2 =-1
      IF (F2 .EQ. FMAX)         K2 =+1
      IF (F2 .GT. FMAX)         K2 =+2
      KK = (K1+2)*5 + (K2+2) + 1
*
**    K2:    -2  -1   0  +1  +2
*                                      K1: -2 -1 0 +1 +2
      GOTO (999,999,200,200,600,
     +      100,100,100,100,500,
     +      400,100,100,100,500,
     +      400,100,100,100,100,
     +      700,300,300,999,999), KK
*
**          1 - S T   P O I N T
*
  100 KPP       = KPP + 1
      PP(1,KPP) = P1(1)
      PP(2,KPP) = P1(2)
      PP(3,KPP) = P1(3)
      GOTO 999
*
**           I N T E R S E C T I O N   W I T H   Fmin
*
  200 KPP       = KPP + 1
      D1        = (FMIN - F1) / (F1 - F2)
      D2        = (FMIN - F2) / (F1 - F2)
      PP(1,KPP) = D2*P1(1) - D1*P2(1)
      PP(2,KPP) = D2*P1(2) - D1*P2(2)
      PP(3,KPP) = D2*P1(3) - D1*P2(3)
      GOTO 999
*
**           I N T E R S E C T I O N   W I T H   Fmax
*
  300 KPP       = KPP + 1
      D1        = (FMAX - F1) / (F1 - F2)
      D2        = (FMAX - F2) / (F1 - F2)
      PP(1,KPP) = D2*P1(1) - D1*P2(1)
      PP(2,KPP) = D2*P1(2) - D1*P2(2)
      PP(3,KPP) = D2*P1(3) - D1*P2(3)
      GOTO 999
*
**          1 - S T   P O I N T,   I N T E R S E C T I O N  WITH  Fmin
*
  400 KPP       = KPP + 1
      PP(1,KPP) = P1(1)
      PP(2,KPP) = P1(2)
      PP(3,KPP) = P1(3)
      KPP       = KPP + 1
      D1        = (FMIN - F1) / (F1 - F2)
      D2        = (FMIN - F2) / (F1 - F2)
      PP(1,KPP) = D2*P1(1) - D1*P2(1)
      PP(2,KPP) = D2*P1(2) - D1*P2(2)
      PP(3,KPP) = D2*P1(3) - D1*P2(3)
      GOTO 999
*
**          1 - S T   P O I N T,   I N T E R S E C T I O N  WITH  Fmax
*
  500 KPP       = KPP + 1
      PP(1,KPP) = P1(1)
      PP(2,KPP) = P1(2)
      PP(3,KPP) = P1(3)
      KPP       = KPP + 1
      D1        = (FMAX - F1) / (F1 - F2)
      D2        = (FMAX - F2) / (F1 - F2)
      PP(1,KPP) = D2*P1(1) - D1*P2(1)
      PP(2,KPP) = D2*P1(2) - D1*P2(2)
      PP(3,KPP) = D2*P1(3) - D1*P2(3)
      GOTO 999
*
**           I N T E R S E C T I O N   W I T H   Fmin, Fmax
*
  600 KPP       = KPP + 1
      D1        = (FMIN - F1) / (F1 - F2)
      D2        = (FMIN - F2) / (F1 - F2)
      PP(1,KPP) = D2*P1(1) - D1*P2(1)
      PP(2,KPP) = D2*P1(2) - D1*P2(2)
      PP(3,KPP) = D2*P1(3) - D1*P2(3)
      KPP       = KPP + 1
      D1        = (FMAX - F1) / (F1 - F2)
      D2        = (FMAX - F2) / (F1 - F2)
      PP(1,KPP) = D2*P1(1) - D1*P2(1)
      PP(2,KPP) = D2*P1(2) - D1*P2(2)
      PP(3,KPP) = D2*P1(3) - D1*P2(3)
      GOTO 999
*
**           I N T E R S E C T I O N   W I T H   Fmax, Fmin
*
  700 KPP       = KPP + 1
      D1        = (FMAX - F1) / (F1 - F2)
      D2        = (FMAX - F2) / (F1 - F2)
      PP(1,KPP) = D2*P1(1) - D1*P2(1)
      PP(2,KPP) = D2*P1(2) - D1*P2(2)
      PP(3,KPP) = D2*P1(3) - D1*P2(3)
      KPP       = KPP + 1
      D1        = (FMIN - F1) / (F1 - F2)
      D2        = (FMIN - F2) / (F1 - F2)
      PP(1,KPP) = D2*P1(1) - D1*P2(1)
      PP(2,KPP) = D2*P1(2) - D1*P2(2)
      PP(3,KPP) = D2*P1(3) - D1*P2(3)
      GOTO 999
*
  999 RETURN
      END
