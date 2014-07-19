*
* $Id: ihmcub.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihmcub.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/06 27/08/93  13.59.54  by  O.Couet
*-- Author :
      SUBROUTINE IHMCUB(FISO,P,F,G,NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMCUB                                     Date:    09.07.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Topological decider for "Matching Cubes" algorithm     *
*               Find set of triangles aproximating the isosurface      *
*               F(x,y,z)=Fiso inside the cube (improved version)       *
*                                                                      *
*     Input: FISO   - function value for isosurface                    *
*            P(3,8) - cube vertexes                                    *
*            F(8)   - function values at the vertexes                  *
*            G(3,8) - function gradients at the vertexes               *
*                                                                      *
*     Output: NNOD       - number of nodes     (maximum 13)            *
*             NTRIA      - number of triangles (maximum 12)            *
*             XYZ(3,*)   - nodes                                       *
*             GRAD(3,*)  - node normales       (not normalized)        *
*             ITRIA(3,*) - triangles                                   *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      PARAMETER         (DEL = 0.0001)
      REAL              P(3,*),F(*),G(3,*),XYZ(3,*),GRAD(3,*)
      INTEGER           ITRIA(3,*),IROTA(8,24),IWHAT(21),ITR(3)
      DATA              IROTA/
     & 1,2,3,4,5,6,7,8, 2,3,4,1,6,7,8,5,
     & 3,4,1,2,7,8,5,6, 4,1,2,3,8,5,6,7,
     & 6,5,8,7,2,1,4,3, 5,8,7,6,1,4,3,2,
     & 8,7,6,5,4,3,2,1, 7,6,5,8,3,2,1,4,
     & 2,6,7,3,1,5,8,4, 6,7,3,2,5,8,4,1,
     & 7,3,2,6,8,4,1,5, 3,2,6,7,4,1,5,8,
     & 5,1,4,8,6,2,3,7, 1,4,8,5,2,3,7,6,
     & 4,8,5,1,3,7,6,2, 8,5,1,4,7,6,2,3,
     & 5,6,2,1,8,7,3,4, 6,2,1,5,7,3,4,8,
     & 2,1,5,6,3,4,8,7, 1,5,6,2,4,8,7,3,
     & 4,3,7,8,1,2,6,5, 3,7,8,4,2,6,5,1,
     & 7,8,4,3,6,5,1,2, 8,4,3,7,5,1,2,6/
      DATA              IWHAT/
     & 1,3,5,65,50,67,74,51,177,105,113,58,165,178,
     & 254,252,250,190,205,188,181/
*-
      NNOD   = 0
      NTRIA  = 0
*
**         F I N D   C O N F I G U R A T I O N   T Y P E
*
      DO 100 I=1,8
        F8(I)  = F(I) - FISO
  100 CONTINUE
      DO 130 IR=1,24
        K      = 0
        INCR   = 1
        DO 110 I=1,8
          IF (F8(IROTA(I,IR)) .GE. 0.)  K = K + INCR
          INCR   = INCR + INCR
  110   CONTINUE
        IF (K.EQ.0 .OR. K.EQ.255)       GOTO 999
        DO 120 I=1,21
          IF (K .NE. IWHAT(I))          GOTO 120
          ICASE  = I
          IRT    = IR
          GOTO 200
  120   CONTINUE
  130 CONTINUE
*
**         R O T A T E   C U B E
*
  200 DO 210 I=1,8
        K      = IROTA(I,IRT)
        F8(I)  = F(K) - FISO
        P8(1,I)= P(1,K)
        P8(2,I)= P(2,K)
        P8(3,I)= P(3,K)
        G8(1,I)= G(1,K)
        G8(2,I)= G(2,K)
        G8(3,I)= G(3,K)
  210 CONTINUE
*
**          V A R I O U S   C O N F I G U R A T I O N S
*
      N = 0
      GOTO (301,302,303,304,305,306,307,308,309,310,311,312,313,314,
     &      301,302,303,304,305,306,307),ICASE
*
  301 CALL IHMC00(1, 4, 9, 0, 0, 0, NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 400
  302 CALL IHMC00(2, 4, 9,10, 0, 0, NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 400
  303 CALL IHMC03(NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 400
  304 CALL IHMC04(NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 400
  305 CALL IHMC00(6, 2, 1, 9, 8, 0, NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 400
  306 CALL IHMC06(NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 400
  307 CALL IHMC07(NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 400
  308 CALL IHMC00(2, 4, 8, 6, 0, 0, NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 500
  309 CALL IHMC00(1, 4,12, 7, 6,10, NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 500
  310 CALL IHMC10(NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 500
  311 CALL IHMC00(1, 4, 8, 7,11,10, NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 500
  312 CALL IHMC12(NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 500
  313 CALL IHMC13(NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 500
  314 CALL IHMC00(1, 9,12, 7, 6, 2, NNOD,NTRIA,XYZ,GRAD,ITRIA)
      GOTO 500
*
**          I F   N E E D E D ,   I N V E R T   T R I A N G L E S
*
  400 IF (NTRIA .EQ. 0)         GOTO 999
      IF (ICASE .LE. 14)        GOTO 500
      DO 410 I=1,NTRIA
        I1     = IABS(ITRIA(1,I))
        I2     = IABS(ITRIA(2,I))
        I3     = IABS(ITRIA(3,I))
        IF (ITRIA(3,I) .LT. 0)  I1 =-I1
        IF (ITRIA(2,I) .LT. 0)  I3 =-I3
        IF (ITRIA(1,I) .LT. 0)  I2 =-I2
        ITRIA(1,I) = I1
        ITRIA(2,I) = I3
        ITRIA(3,I) = I2
  410 CONTINUE
*
**          R E M O V E   V E R Y   S M A L L   T R I A N G L E S
*
  500 N      = N + 1
  510 IF (N .GT. NTRIA)         GOTO 999
      DO 520 I=1,3
        I1     = I
        I2     = I + 1
        IF (I .EQ. 3)           I2 = 1
        K1     = IABS(ITRIA(I1,N))
        K2     = IABS(ITRIA(I2,N))
        IF (ABS(XYZ(1,K1)-XYZ(1,K2)) .GT. DEL)  GOTO 520
        IF (ABS(XYZ(2,K1)-XYZ(2,K2)) .GT. DEL)  GOTO 520
        IF (ABS(XYZ(3,K1)-XYZ(3,K2)) .GT. DEL)  GOTO 520
        I3     = I - 1
        IF (I .EQ. 1)           I3 = 3
        GOTO 530
  520 CONTINUE
      GOTO 500
*           R E M O V E   T R I A N G L E
  530  DO 540 I=1,3
       ITR(I)     = ITRIA(I,N)
       ITRIA(I,N) = ITRIA(I,NTRIA)
  540 CONTINUE
      NTRIA = NTRIA - 1
      IF (NTRIA .EQ. 0)                 GOTO 999
      IF (ITR(I2)*ITR(I3) .GT. 0)       GOTO 510
*          C O R R E C T   O T H E R   T R I A N G L E S
      IF (ITR(I2) .LT. 0) THEN
        K1     =-ITR(I2)
        K2     =-IABS(ITR(I3))
      END IF
      IF (ITR(I3) .LT. 0) THEN
        K1     =-ITR(I3)
        K2     =-IABS(ITR(I1))
      END IF
      DO 560 J=1,NTRIA
        DO 550 I=1,3
          IF (ITRIA(I,J) .NE. K2)       GOTO 550
          I2     = IABS(ITRIA(1,J))
          IF (I .NE. 3)                 I2 = IABS(ITRIA(I+1,J))
          IF (I2 .EQ. K1)               ITRIA(I,J) =-ITRIA(I,J)
          GOTO 560
  550   CONTINUE
  560 CONTINUE
      GOTO 510
*
  999 RETURN
      END
