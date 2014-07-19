*
* $Id: igbrik.F,v 1.1.1.1 1996/02/14 13:10:42 mclareni Exp $
*
* $Log: igbrik.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGBRIK(LIGHT,T,DX,DY,DZ)
************************************************************************
*                                                                      *
* Name: IGBRIK                                     Date:    10.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Draw 3D box                                                *
*                                                                      *
************************************************************************
      INTEGER IFACE(4,6)
      REAL    T(4,3),P(3,8),ANORM(3,6),AN(3)
      REAL    X(4),Y(4),Z(4),F(4)
      DATA    IFACE /1,2,6,5,    2,3,7,6,    3,4,8,7,   4,1,5,8,
     +               4,3,2,1,    5,6,7,8/
      DATA    ANORM /0.,-1.,0.,  1.,0.,0.,  0.,1.,0.,  -1.,0.,0.,
     +               0.,0.,-1.,  0.,0.,1./
*-
**       S E T   V E R T I C E S
*
      P(1,1) = 0.
      P(2,1) = 0.
      P(1,2) = DX
      P(2,2) = 0.
      P(1,3) = DX
      P(2,3) = DY
      P(1,4) = 0.
      P(2,4) = DY
      DO 100 I=1,4
        P(3,I)   = 0.
        P(1,I+4) = P(1,I)
        P(2,I+4) = P(2,I)
        P(3,I+4) = DZ
  100 CONTINUE
*
**      T R A N S F O R M   V E R T I C E S
*
      DO 200 I=1,8
        CALL IGTRAN(P(1,I),T,P(1,I))
  200 CONTINUE
*
**      D R A W   F A C E S
*
      DO 320 I=1,6
        DO 310 K=1,4
          N    = IFACE(K,I)
          X(K) = P(1,N)
          Y(K) = P(2,N)
          Z(K) = P(3,N)
  310   CONTINUE
        IF (LIGHT .NE. 0) THEN
          CALL IGTNOR(ANORM(1,I),T,AN)
          CALL IHLUMI(AN,F(1))
          F(2) = F(1)
          F(3) = F(1)
          F(4) = F(1)
          CALL IFAF3(4,X,Y,Z,F)
        ELSE
          CALL IFA3(4,X,Y,Z)
        END IF
  320 CONTINUE
      END
