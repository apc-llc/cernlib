*
* $Id: igtrot.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: igtrot.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTROT(ANG,P1,P2,T)
************************************************************************
*                                                                      *
* Name: IGTROT                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Add to matrix T the rotation though ANG (counterclockwise) *
*           around an axis P1->P2                                      *
*                                                                      *
************************************************************************
      REAL P1(3),P2(3),T(4,3)
      DOUBLE PRECISION DX,DY,DZ,CX,CY,CZ,S,RAD,COSA,SINA
      DOUBLE PRECISION DT1(4,4),DT2(4,3)
*-
      DX     = P1(1)
      DY     = P1(2)
      DZ     = P1(3)
      CX     = P2(1) - P1(1)
      CY     = P2(2) - P1(2)
      CZ     = P2(3) - P1(3)
      S      = DSQRT(CX*CX + CY*CY + CZ*CZ)
      SS     = S
      IF (SS .EQ. 0.) GOTO 999
      CX     = CX / S
      CY     = CY / S
      CZ     = CZ / S
      RAD    = DATAN(1.0D+00)*4./180.0
      COSA   = DCOS(ANG*RAD)
      SINA   = DSIN(ANG*RAD)
*
      DT2(1,1) = COSA + (1.-COSA)*CX*CX
      DT2(2,1) =        (1.-COSA)*CX*CY - SINA*CZ
      DT2(3,1) =        (1.-COSA)*CX*CZ + SINA*CY
      DT2(4,1) = DX - DT2(1,1)*DX - DT2(2,1)*DY - DT2(3,1)*DZ
*
      DT2(1,2) =        (1.-COSA)*CY*CX + SINA*CZ
      DT2(2,2) = COSA + (1.-COSA)*CY*CY
      DT2(3,2) =        (1.-COSA)*CY*CZ - SINA*CX
      DT2(4,2) = DY - DT2(1,2)*DX - DT2(2,2)*DY - DT2(3,2)*DZ
*
      DT2(1,3) =        (1.-COSA)*CZ*CX - SINA*CY
      DT2(2,3) =        (1.-COSA)*CZ*CY + SINA*CX
      DT2(3,3) = COSA + (1.-COSA)*CZ*CZ
      DT2(4,3) = DZ - DT2(1,3)*DX - DT2(2,3)*DY - DT2(3,3)*DZ
*
      DO 120 I=1,4
        DO 110 K=1,3
          DT1(I,K) = T(I,K)
  110   CONTINUE
        DT1(I,4) = 0.
  120 CONTINUE
      DT1(4,4) = 1.
*
      DO 220 I=1,4
        DO 210 K=1,3
          T(I,K) = DT1(I,1)*DT2(1,K) + DT1(I,2)*DT2(2,K) +
     +             DT1(I,3)*DT2(3,K) + DT1(I,4)*DT2(4,K)
  210   CONTINUE
  220 CONTINUE
*
  999 RETURN
      END
