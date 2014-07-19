*
* $Id: igtor.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: igtor.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTOR(LIGHT,T,R1,R2,N1,N2)
************************************************************************
*                                                                      *
* Name: IGTOR                                      Date:    10.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Draw torus                                                 *
*                                                                      *
************************************************************************
      REAL T(4,3),P(3,4),ANORM(3,4),X(4),Y(4),Z(4),F(4)
*-
      IF (N1.LT.3 .OR. N2.LT.3) RETURN
      RAD   = ATAN(1.)*4./180.
      A     = RAD*360./N1
      B     = RAD*360./N2
      DO 400 I1=1,N1
        COSA1 = COS((I1-1)*A)
        SINA1 = SIN((I1-1)*A)
        COSA2 = COS(I1*A)
        SINA2 = SIN(I1*A)
        IF (I1 .EQ. 1) THEN
          COSA1 = 1.
          SINA1 = 0.
        END IF
        IF (I1 .EQ. N1) THEN
          COSA2 = 1.
          SINA2 = 0.
        END IF
        DO 300 I2=1,N2
          COSB1 = COS((I2-1)*B)
          SINB1 = SIN((I2-1)*B)
          COSB2 = COS(I2*B)
          SINB2 = SIN(I2*B)
          IF (I2 .EQ. 1) THEN
            COSB1 = 1.
            SINB1 = 0.
          END IF
          IF (I2 .EQ. N2) THEN
            COSB2 = 1.
            SINB2 = 0.
          END IF
*
**      F I N D   N O R M A L  E S
*
          ANORM(1,1) = COSA1*COSB1
          ANORM(2,1) = SINA1*COSB1
          ANORM(3,1) = SINB1

          ANORM(1,2) = COSA2*COSB1
          ANORM(2,2) = SINA2*COSB1
          ANORM(3,2) = SINB1

          ANORM(1,3) = COSA2*COSB2
          ANORM(2,3) = SINA2*COSB2
          ANORM(3,3) = SINB2

          ANORM(1,4) = COSA1*COSB2
          ANORM(2,4) = SINA1*COSB2
          ANORM(3,4) = SINB2
*
**      F I N D   C O O R D I N A T E S   O F   V E R T I C E S
*
          P(1,1) = ANORM(1,1)*R2 + R1*COSA1
          P(2,1) = ANORM(2,1)*R2 + R1*SINA1
          P(3,1) = ANORM(3,1)*R2

          P(1,2) = ANORM(1,2)*R2 + R1*COSA2
          P(2,2) = ANORM(2,2)*R2 + R1*SINA2
          P(3,2) = ANORM(3,2)*R2

          P(1,3) = ANORM(1,3)*R2 + R1*COSA2
          P(2,3) = ANORM(2,3)*R2 + R1*SINA2
          P(3,3) = ANORM(3,3)*R2

          P(1,4) = ANORM(1,4)*R2 + R1*COSA1
          P(2,4) = ANORM(2,4)*R2 + R1*SINA1
          P(3,4) = ANORM(3,4)*R2
          DO 100 I=1,4
            CALL IGTRAN(P(1,I),T,P(1,I))
  100     CONTINUE
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
