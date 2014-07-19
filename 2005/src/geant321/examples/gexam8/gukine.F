*
* $Id: gukine.F,v 1.2 1996/02/22 13:26:56 ravndal Exp $
*
* $Log: gukine.F,v $
* Revision 1.2  1996/02/22 13:26:56  ravndal
* Cleaning up CARTOCVS interface
*
* Revision 1.1.1.1  1995/10/24 10:22:22  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUKINE
C.
C.    *
C.    *       Generates Kinematics for primary track
C.    *
C.
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "pvolum.inc"
#include "geant321/gconsp.inc"
C
 
      DIMENSION VERTEX(3),PLAB(3),RNDM(2)
C.
C.
      DO 10 I=1,6
         ETOT(I)=0.
         ESEEN(I)=0.
  10  CONTINUE
      VERTEX(1) = 0.
      VERTEX(2) = 0.
      VERTEX(3) = - 0.9999*ZECAL
      IF(IKINE.GT.100)THEN
         IK=IKINE-100
         THETA=PKINE(2)*DEGRAD
         PHI=PKINE(3)*DEGRAD
      ELSE
         IK=IKINE
         CALL GRNDM(RNDM,2)
         THETA=PI*RNDM(1)
         PHI=TWOPI*RNDM(2)
      ENDIF
C
      PLAB(1) = PKINE(1)*SIN(THETA)*COS(PHI)
      PLAB(2) = PKINE(1)*SIN(THETA)*SIN(PHI)
      PLAB(3) = PKINE(1)*COS(THETA)
C
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
      CALL GSKINE(PLAB,IK,NVERT,0,0,NT)
      JPA=LQ(JPART-IK)
      ITR=Q(JPA+6)
 
C
C ***          Kinematics debug (controlled by ISWIT(1) )
C
      IF(IDEBUG.EQ.1) THEN
        IF(ISWIT(1).EQ.1) THEN
          CALL GPRINT('VERT',0)
          CALL GPRINT('KINE',0)
        ENDIF
      ENDIF
 
C
      END
