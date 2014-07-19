*
* $Id: igsphe.F,v 1.1.1.1 1996/02/14 13:10:42 mclareni Exp $
*
* $Log: igsphe.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGSPHE(LIGHT,T,R,N1,N2)
************************************************************************
*                                                                      *
* Name: IGSPHE                                     Date:    10.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Draw sphere                                                *
*                                                                      *
************************************************************************
      REAL T(4,3),P(3,4),ANORM(3,4),X(4),Y(4),Z(4),F(4)
*-
      IF (N1.LT.2 .OR. N2.LT.3) RETURN
      RAD   = ATAN(1.)*4./180.
      THETA = RAD*180./N1
      PHI   = RAD*360./N2
      DO 400 ITHETA=1,N1
        COST1 = COS((ITHETA-1)*THETA)
        SINT1 = SIN((ITHETA-1)*THETA)
        COST2 = COS(ITHETA*THETA)
        SINT2 = SIN(ITHETA*THETA)
        IF (ITHETA .EQ. 1) THEN
          COST1 = 1.
          SINT1 = 0.
        END IF
        IF (ITHETA .EQ. N1) THEN
          COST2 =-1.
          SINT2 = 0.
        END IF
        DO 300 IPHI=1,N2
          COSP1 = COS((IPHI-1)*PHI)
          SINP1 = SIN((IPHI-1)*PHI)
          COSP2 = COS(IPHI*PHI)
          SINP2 = SIN(IPHI*PHI)
          IF (IPHI .EQ. 1) THEN
            COSP1 = 1.
            SINP1 = 0.
          END IF
          IF (IPHI .EQ. N2) THEN
            COSP2 = 1.
            SINP2 = 0.
          END IF
*
**      F I N D   N O R M A L  E S
*
          ANORM(1,1) = SINT1*COSP1
          ANORM(2,1) = SINT1*SINP1
          ANORM(3,1) = COST1

          ANORM(1,2) = SINT2*COSP1
          ANORM(2,2) = SINT2*SINP1
          ANORM(3,2) = COST2

          ANORM(1,3) = SINT2*COSP2
          ANORM(2,3) = SINT2*SINP2
          ANORM(3,3) = COST2

          ANORM(1,4) = SINT1*COSP2
          ANORM(2,4) = SINT1*SINP2
          ANORM(3,4) = COST1
*
**      F I N D   C O O R D I N A T E S   O F   V E R T I C E S
*
          DO 120 I=1,4
            DO 110 K=1,3
              P(K,I) = ANORM(K,I)*R
  110       CONTINUE
            CALL IGTRAN(P(1,I),T,P(1,I))
  120     CONTINUE
*
**      D R A W   F A C E
*
          DO 200 I=1,4
            X(I) = P(1,I)
            Y(I) = P(2,I)
            Z(I) = P(3,I)
            IF (LIGHT .NE. 0) THEN
              CALL IGTNOR(ANORM(1,I),T,ANORM(1,I))
              CALL IHLUMI(ANORM(1,I),F(I))
            END IF
  200     CONTINUE
          IF (LIGHT .NE. 0)  THEN
            CALL IFAF3(4,X,Y,Z,F)
          ELSE
            CALL IFA3(4,X,Y,Z)
          END IF
  300   CONTINUE
  400 CONTINUE
      END
