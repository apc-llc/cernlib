*
* $Id: igcone.F,v 1.1.1.1 1996/02/14 13:10:42 mclareni Exp $
*
* $Log: igcone.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGCONE(LIGHT,T,H,R1,R2,NSTEP)
************************************************************************
*                                                                      *
* Name: IGCONE                                     Date:    10.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Draw cone                                                  *
*                                                                      *
************************************************************************
      PARAMETER (NMAX=72)
      REAL      T(4,3),P1(3,NMAX+1),P2(3,NMAX+1)
      REAL      X(NMAX),Y(NMAX),Z(NMAX),F(NMAX),ANORM(3)
*-
      IF (NSTEP.LT.3 .OR. NSTEP.GT.NMAX) RETURN
      ANG = ATAN(1.)*8. / NSTEP
*
**       S E T   V E R T I C E S
*
      DO 100 I=1,NSTEP+1
        IF (I.EQ.1 .OR. I.EQ.NSTEP+1) THEN
          COSA   = 1.
          SINA   = 0.
        ELSE
          COSA   = COS((I-1)*ANG)
          SINA   = SIN((I-1)*ANG)
        END IF
        P1(1,I) = R1*COSA
        P1(2,I) = R1*SINA
        P1(3,I) = 0.
        P2(1,I) = R2*COSA
        P2(2,I) = R2*SINA
        P2(3,I) = H
  100 CONTINUE
*
**      T R A N S F O R M   V E R T I C E S
*
      DO 200 I=1,NSTEP+1
        CALL IGTRAN(P1(1,I),T,P1(1,I))
        CALL IGTRAN(P2(1,I),T,P2(1,I))
  200 CONTINUE
*
**      D R A W   L O W E R   F A C E
*
      IF (R1 .NE. 0.) THEN
        IF (LIGHT .NE. 0) THEN
          ANORM(1) = 0.
          ANORM(2) = 0.
          ANORM(3) =-1.
          CALL IGTNOR(ANORM,T,ANORM)
          CALL IHLUMI(ANORM,F(1))
        END IF
        DO 310 I=1,NSTEP
          X(I) = P1(1,NSTEP+1-I)
          Y(I) = P1(2,NSTEP+1-I)
          Z(I) = P1(3,NSTEP+1-I)
          IF (LIGHT .NE. 0) F(I) = F(1)
  310   CONTINUE
        IF (LIGHT .NE. 0) THEN
          CALL IFAF3(NSTEP,X,Y,Z,F)
        ELSE
          CALL IFA3(NSTEP,X,Y,Z)
        END IF
      END IF
*
**      D R A W   U P P E R   F A C E
*
      IF (R1 .NE. 0.) THEN
        IF (LIGHT .NE. 0) THEN
          ANORM(1) = 0.
          ANORM(2) = 0.
          ANORM(3) = 1.
          CALL IGTNOR(ANORM,T,ANORM)
          CALL IHLUMI(ANORM,F(1))
        END IF
        DO 320 I=1,NSTEP
          X(I) = P2(1,I)
          Y(I) = P2(2,I)
          Z(I) = P2(3,I)
          IF (LIGHT .NE. 0) F(I) = F(1)
  320   CONTINUE
        IF (LIGHT .NE. 0) THEN
          CALL IFAF3(NSTEP,X,Y,Z,F)
        ELSE
          CALL IFA3(NSTEP,X,Y,Z)
        END IF
      END IF
*
**      D R A W   S I D E   F A C E S
*
      DO 400 I=1,NSTEP
        X(1) = P1(1,I)
        Y(1) = P1(2,I)
        Z(1) = P1(3,I)
        X(2) = P1(1,I+1)
        Y(2) = P1(2,I+1)
        Z(2) = P1(3,I+1)
        X(3) = P2(1,I+1)
        Y(3) = P2(2,I+1)
        Z(3) = P2(3,I+1)
        X(4) = P2(1,I)
        Y(4) = P2(2,I)
        Z(4) = P2(3,I)
        IF (LIGHT .NE. 0) THEN
          ANORM(1) = COS((I-1)*ANG)
          ANORM(2) = SIN((I-1)*ANG)
          ANORM(3) = 0.
          CALL IGTNOR(ANORM,T,ANORM)
          CALL IHLUMI(ANORM,F(1))
          F(4) = F(1)
          ANORM(1) = COS(I*ANG)
          ANORM(2) = SIN(I*ANG)
          ANORM(3) = 0.
          CALL IGTNOR(ANORM,T,ANORM)
          CALL IHLUMI(ANORM,F(2))
          F(3) = F(2)
          CALL IFAF3(4,X,Y,Z,F)
        ELSE
          CALL IFAF3(4,X,Y,Z)
        END IF
  400 CONTINUE
      END
