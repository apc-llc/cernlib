*
* $Id: gukine.F,v 1.2 1996/02/22 13:32:38 ravndal Exp $
*
* $Log: gukine.F,v $
* Revision 1.2  1996/02/22 13:32:38  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:24  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUKINE
*
************************************************************************
*                                                                      *
*             GEANT3 user routine to generate Kinematics               *
*               for primary tracks                                     *
*                                                                      *
************************************************************************
*
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "urgeom.inc"
#include "tstcom.inc"
#include "hboot.inc"
*
      DIMENSION VERTEX(3), PLAB(3), BUF(10), MHE1(5)
      DATA NBUF /1/, BUF /10*0.0/
      DATA MHE1 /4HJHE1, 1, 1, 20, 3/, NZERO /0/
*
      IF (LHBOOT(1) .EQ. 1) THEN
 
*             Define "JHE1" bank to be saved
         JHE1 = LQ(JHEAD-1)
         IF (JHE1.NE.0) CALL MZDROP (IXSTOR, JHE1, '    ' )
         CALL MZLIFT (IXSTOR, JHE1, JHEAD, -1, MHE1, NZERO)
   10    CALL RANNOR(RANX, RANY)
         XYZ(1) = XY0(1) + RANX * SIGBX
         XYZ(2) = XY0(2) + RANY * SIGBY
         IF(ABS(XYZ(1)).GT.BSPOT.OR.ABS(XYZ(2)).GT.BSPOT) GO TO 10
         XYZ(3) = XY0(3)
         PXYZ(1) = PPART*SIN(ANGL(2))*COS(ANGL(1))
         PXYZ(2) = PPART*SIN(ANGL(2))*SIN(ANGL(1))
         PXYZ(3) = PPART*COS(ANGL(2))
*
      ELSE
*
         RANX=RNDM(DUMMY)
         RANY=RNDM(DUMMY)
         XYZ(1) = XY0(1) + RANX * SIGBX
         XYZ(2) = XY0(2) + RANY * SIGBY
*
   20    RANZ=RNDM(DUMMY)
         DZ = RANZ * SIGBZ
   30    XYZ(3) = XY0(3) + DZ
         CALL HFILL(25,DZ,0.,1.)
*
* *** Energy of the incident particle
*
         ISHOW = ISHOW+1
         IF(ISHOW.GT.NSHOW(IENERG)) THEN
            IF(ITHETA.EQ.9) THEN
               CALL SUMANG
               IENERG = IENERG+1
               ISHOW = 1
               ITHETA = 0
            ELSE
               ITHETA = ITHETA +1
               ISHOW = 1
            ENDIF
         ENDIF
*
         IF(ISHOW.EQ.1) THEN
            JPA = LQ(JPART-KPART)
            AMASS = Q(JPA+7)
            EKINE = ESHOW(IENERG)
            PPART=SQRT((EKINE+2.*AMASS)*EKINE)
            WRITE(6,10000) IENERG,ITHETA,EKINE,PPART,IEVENT
10000  FORMAT('   IENERG',I4,'   ITHETA',I4,'   EKINE,PPART',
     +        2F9.5,'   IEVENTS',I8)
         ENDIF
*
         THET = 3.1415926535/18.*ITHETA
         PXYZ(1) = PPART*SIN(THET)
         PXYZ(2) = 0.0
         PXYZ(3) = PPART*COS(THET)
*
         LHBEG=0
      END IF
 
      CALL GSVERT(XYZ,0,0,0,0,NVERT)
      CALL GSKINE(PXYZ,KPART,NVERT,BUF,NBUF,NT)
*
*              Kinematic debug (controled by ISWIT(1))
      IF(IDEBUG.EQ.1.AND.ISWIT(1).EQ.1) THEN
         CALL GPRINT('VERT',0)
         CALL GPRINT('KINE',0)
      ENDIF
*
      END
