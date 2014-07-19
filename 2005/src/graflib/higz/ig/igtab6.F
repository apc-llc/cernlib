*
* $Id: igtab6.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igtab6.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/02 06/07/93  09.40.04  by  O.Couet
*-- Author : E.Chernyaev
      SUBROUTINE IGTAB6(IX,IY,FACE,T)
************************************************************************
*                                                                      *
*     Function: Find part of surface with luminosity in the corners    *
*               This routine is used for Gouraud shading               *
*                                                                      *
************************************************************************
#include "higz/hipack.inc"
#include "higz/hihid.inc"
      REAL      FACE(3,4),T(4)
      REAL      F(3,4,3,3),X(4,3,3),Y(4,3,3),Z(4,3,3)
      REAL      AN(3,3,3),BN(3,2,2)
      INTEGER   INCRX(3),INCRY(3)
*-
      IPHI = 1
      RAD  = ATAN(1.)*4./180.
*
**          Find real cell indexes
*
      IXT = IX + IXFCHA(1) - 1
      IYT = IY + IYFCHA(1) - 1
*
**          Find increments of neighboring cells
*
      INCRX(1) =-1
      INCRX(2) = 0
      INCRX(3) =+1
      IF (IXT .EQ. 1)           INCRX(1) = 0
      IF (IXT .EQ. NCX-1)       INCRX(3) = 0
      INCRY(1) =-1
      INCRY(2) = 0
      INCRY(3) =+1
      IF (IYT .EQ. 1)           INCRY(1) = 0
      IF (IYT .EQ. NCY-1)       INCRY(3) = 0
*
**          Find neighboring faces
*
      DO 120 J=1,3
        DO 110 I=1,3
          CALL IGTAB2(IX+INCRX(I),IY+INCRY(J),F(1,1,I,J),T)
  110   CONTINUE
  120 CONTINUE
*
**         Set face
*
      DO 220 K=1,4
        DO 210 I=1,3
          FACE(I,K) = F(I,K,2,2)
  210   CONTINUE
  220 CONTINUE
*
**         Find coordinates and normales
*
      DO 330 J=1,3
        DO 320 I=1,3
          DO 310 K=1,4
* Polar
            IF (ISYS.EQ.2) THEN
              PHI      = F(IPHI,K,I,J)*RAD
              R        = F(3-IPHI,K,I,J)
              X(K,I,J) = R*COS(PHI)
              Y(K,I,J) = R*SIN(PHI)
              Z(K,I,J) = F(3,K,I,J)
* Cylindrical
            ELSE IF (ISYS.EQ.3) THEN
              PHI      = F(IPHI,K,I,J)*RAD
              R        = F(3,K,I,J)
              X(K,I,J) = R*COS(PHI)
              Y(K,I,J) = R*SIN(PHI)
              Z(K,I,J) = F(3-IPHI,K,I,J)
* Spherial
            ELSE IF (ISYS.EQ.4) THEN
              PHI      = F(IPHI,K,I,J)*RAD
              TH       = F(3-IPHI,K,I,J)*RAD
              R        = F(3,K,I,J)
              X(K,I,J) = R*SIN(TH)*COS(PHI)
              Y(K,I,J) = R*SIN(TH)*SIN(PHI)
              Z(K,I,J) = R*COS(TH)
* Pseudo ..
            ELSE IF (ISYS.EQ.5) THEN
              PHI      = F(IPHI,K,I,J)*RAD
              TH       = F(3-IPHI,K,I,J)*RAD
              R        = F(3,K,I,J)
              X(K,I,J) = R*COS(PHI)
              Y(K,I,J) = R*SIN(PHI)
              Z(K,I,J) = R*COS(TH)/SIN(TH)
* Carthesian
            ELSE
              X(K,I,J) = F(1,K,I,J)
              Y(K,I,J) = F(2,K,I,J)
              Z(K,I,J) = F(3,K,I,J)
            END IF
  310     CONTINUE
          X1 = X(3,I,J) - X(1,I,J)
          X2 = X(4,I,J) - X(2,I,J)
          Y1 = Y(3,I,J) - Y(1,I,J)
          Y2 = Y(4,I,J) - Y(2,I,J)
          Z1 = Z(3,I,J) - Z(1,I,J)
          Z2 = Z(4,I,J) - Z(2,I,J)
          AN(1,I,J) = (Y1*Z2 - Y2*Z1)
          AN(2,I,J) = (Z1*X2 - Z2*X1)
          AN(3,I,J) = (X1*Y2 - X2*Y1)
          S = SQRT(AN(1,I,J)*AN(1,I,J)+
     +             AN(2,I,J)*AN(2,I,J)+
     +             AN(3,I,J)*AN(3,I,J))
          AN(1,I,J) = AN(1,I,J) / S
          AN(2,I,J) = AN(2,I,J) / S
          AN(3,I,J) = AN(3,I,J) / S
  320   CONTINUE
  330 CONTINUE
*
**          Find average normales
*
      DO 430 J=1,2
        DO 420 I=1,2
          DO 410 K=1,3
            BN(K,I,J) = AN(K,I,J)+AN(K,I+1,J)+AN(K,I+1,J+1)+AN(K,I,J+1)
  410     CONTINUE
  420   CONTINUE
  430 CONTINUE
*
**          Set luminosity
*
      CALL IHLUMI(BN(1,1,1),T(1))
      CALL IHLUMI(BN(1,2,1),T(2))
      CALL IHLUMI(BN(1,2,2),T(3))
      CALL IHLUMI(BN(1,1,2),T(4))
*
      END
