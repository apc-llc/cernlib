*
* $Id: gustep.F,v 1.2 1996/02/22 13:26:58 ravndal Exp $
*
* $Log: gustep.F,v $
* Revision 1.2  1996/02/22 13:26:58  ravndal
* Cleaning up CARTOCVS interface
*
* Revision 1.1.1.1  1995/10/24 10:22:23  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
C.
C.    *
C.    *       User routine called at the end of each tracking step
C.    *       INWVOL is different from 0 when the track has reached
C.    *              a volume boundary
C.    *       ISTOP is different from 0 if the track has stopped
C.    *
C.
#include "geant321/gcflag.inc"
#include "geant321/gctmed.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcnum.inc"
#include "pvolum.inc"
      DIMENSION X(12)
      common/temp/dedx0,dedx1,dedx2,dedx3,ener
C              Something generated ?
 
      IF(NGKINE.GT.0) THEN
        DO 30 I=1,NGKINE
          ITYPA  = GKIN(5,I)
          IF(ITYPA.NE.4)  CALL GSKING(I)
   30   CONTINUE
      ENDIF
      IF(NUMED.EQ.2) THEN
       IF(DESTEP.NE.0.)THEN
          CALL GBIRK(EDEP)
          R=SQRT(VECT(1)**2+VECT(2)**2)
          Z=VECT(3)
          CALL HF1(30+ITR,Z,EDEP)
          CALL HF1(40+ITR,R,EDEP)
          IF(ITRTYP.EQ.5)CALL HF1(65,EDEP,1.)
          ESEEN(ITR)=ESEEN(ITR)+EDEP
       ENDIF
       IF(ISWIT(8).NE.0)THEN
          X(1)=IEVENT
          X(2)=IPART
          X(3)=GEKIN
          X(4)=ISTOP
          X(5)=INWVOL
          X(6)=STEP
          X(7)=DESTEP
          X(8)=DXMIP
          X(9)=TOFG
          X(10)=Z
*          CALL HFN(1,X)
          if(itrtyp.eq.2)then
          X(1)=IEVENT
          X(2)=IPART
          X(3)=GEKIN
          X(4)=ISTOP
          X(5)=INWVOL
          X(6)=STEP
          X(7)=DESTEP
          X(8)=ener
          X(9)=dedx0
          X(10)=dedx1
          X(11)=dedx2
          X(12)=dedx3
          CALL HFN(2,X)
          ENDIF
       ENDIF
      ENDIF
      ETOT(ITR)=ETOT(ITR)+DESTEP
*
*             Muon range
*
      IF(ITR.EQ.5)THEN
         IF(ISTOP.NE.0.AND.NTMULT.EQ.1)THEN
            CALL HF1(51,SLENG,1.)
         ENDIF
      ENDIF
 
 
C             Debug event
 
      CALL GDEBUG
 
      END
