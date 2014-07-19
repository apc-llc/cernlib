*
* $Id: igtab4.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igtab4.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/08 24/02/95  15.44.06  by  O.Couet
*-- Author :    O.Couet   07/01/92
      SUBROUTINE IGTAB4(RMIN,RMAX,RBACK)
*.===========>
*.
*.  Set the correct window size for lego and surface plots.
*   And draw the background if necessary.
*.
*. _Input parameters:
*.
*.  REAL RMIN(3), RMAX(3) : 3D coordinates of the box.
*.  REAL RBACK : Background colour
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hctnor.inc"
#include "higz/higraf.inc"
      DIMENSION RMIN(*),RMAX(*)
      DIMENSION SMAX(2)
*.______________________________________
*
      DO 20 I=1,2
         SMAX(I) = TNORM(4,I)
         DO 10 K=1,3
            IF (TNORM(K,I) .LT. 0.) THEN
               SMAX(I) = SMAX(I) + TNORM(K,I)*RMIN(K)
            ELSE
               SMAX(I) = SMAX(I) + TNORM(K,I)*RMAX(K)
            ENDIF
   10    CONTINUE
   20 CONTINUE
      CALL ISWN(INTR,-SMAX(1),SMAX(1),-SMAX(2),SMAX(2))
      CALL ISELNT(INTR)
*
*              Draw the background if necessary
*
      IF(RBACK.GE.1000.)THEN
         R1=-1.
         R2=-1.
         R3=-1.
         XGRAF(2)=-SMAX(1)
         XGRAF(3)=-SMAX(1)
         XGRAF(5)=+SMAX(1)
         XGRAF(6)=+SMAX(1)
         YGRAF(1)=-SMAX(2)
         YGRAF(4)=+SMAX(2)
         YGRAF(2)=+SMAX(2)
         YGRAF(3)=-SMAX(2)
         YGRAF(6)=+SMAX(2)
         YGRAF(5)=-SMAX(2)
         DO 30 I=1,8
            X=((1.-R1)*RMIN(1)+(R1+1.)*RMAX(1))/2.
            Y=((1.-R2)*RMIN(2)+(R2+1.)*RMAX(2))/2.
            Z=((1.-R3)*RMIN(3)+(R3+1.)*RMAX(3))/2.
            XX=TNORM(1,1)*X+TNORM(2,1)*Y+TNORM(3,1)*Z+TNORM(4,1)
            YY=TNORM(1,2)*X+TNORM(2,2)*Y+TNORM(3,2)*Z+TNORM(4,2)
            IF(ABS(XX-XGRAF(2)).LE.0.0001)THEN
               IF(YGRAF(2).GE.YY)YGRAF(2)=YY
               IF(YGRAF(3).LE.YY)YGRAF(3)=YY
            ENDIF
            IF(ABS(XX-XGRAF(6)).LE.0.0001)THEN
               IF(YGRAF(6).GE.YY)YGRAF(6)=YY
               IF(YGRAF(5).LE.YY)YGRAF(5)=YY
            ENDIF
            IF(ABS(YY-YGRAF(1)).LE.0.0001)XGRAF(1)=XX
            IF(ABS(YY-YGRAF(4)).LE.0.0001)XGRAF(4)=XX
            R1=-R1
            IF(MOD(I,2).EQ.0)R2=-R2
            IF(I.GE.4)R3=1
   30    CONTINUE
*
         IFACS = IFACI
         CALL ISFAIS(1)
         ICOL=INT(RBACK-100.*FLOAT(INT(RBACK/100.)))
         CALL ISFACI(ICOL)
         CALL IFA(6,XGRAF,YGRAF)
         CALL ISFACI(IFACS)
      ENDIF
*
      END
