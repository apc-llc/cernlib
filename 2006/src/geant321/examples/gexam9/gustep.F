*
* $Id: gustep.F,v 1.2 1996/02/22 13:32:39 ravndal Exp $
*
* $Log: gustep.F,v $
* Revision 1.2  1996/02/22 13:32:39  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:24  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
*
************************************************************************
*                                                                      *
*        GEANT3 user routine called at the end of each tracking step   *
*                                                                      *
************************************************************************
*
#include "geant321/gctmed.inc"
#include "geant321/gcking.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
#include "geant321/gcsets.inc"
#include "geant321/gcbank.inc"
#include "geant321/gconst.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcunit.inc"
#include "geant321/gclist.inc"
#include "tstcom.inc"
#include "hboot.inc"
      INTEGER HNAME(4),ECAL,HCAL,ECRY,HSCI
      EQUIVALENCE (ECAL,HNAME(1)),(HCAL,HNAME(2))
      EQUIVALENCE (ECRY,HNAME(3)),(HSCI,HNAME(4))
      CHARACTER*4 MYNAME(4)
      DIMENSION KTRT(5),HITS(1)
      DATA KTRT/5*0/,IFIRST/0/
      DATA TOMAX/2.E-7/
      DATA MYNAME / 'ECAL','HCAL','ECRY','HSCI'/
*
      IF (IFIRST.EQ.0) THEN
         CALL UCTOH(MYNAME,HNAME,4,16)
         IFIRST=1
      END IF
*
*             Something generated ?
      IF(NGKINE.GT.0) THEN
         DO 10 I=1,NGKINE
            ITYPA = INT(GKIN(5,I))
*
*     BOOTSTRAP THE SECONDARY PARTICLES.
            IF(LHBOOT(1).EQ.1.AND.VECT(3).GT.-6.4) THEN
               IF(ITYPA.GE.2.AND.ITYPA.LE.3.AND. GKIN(4,I).LE.EBTUP(1))
     +         THEN
                  CALL HBOOTG(1,I,IBOOT)
                  IF(IBOOT.EQ.1) THEN
                     CALL HFILL(31,GKIN(4,I),0.,1.)
                     GOTO 10
                  ENDIF
               END IF
               IF(LHBOOT(2).EQ.1) THEN
                  IF(ITYPA.GE.8.AND.ITYPA.LE.9.AND. (GKIN(4,I))
     +            .LE.EBTUP(2)) THEN
                     CALL HBOOTG(2,I,IBOOT)
                     IF(IBOOT.EQ.1) THEN
                        CALL HFILL(41,GKIN(4,I),0.,1.)
                        GOTO 10
                     ENDIF
                  END IF
               ENDIF
            ENDIF
            IF(ITYPA.NE.4) CALL GSKING(I)
   10    CONTINUE
      END IF
*
*             Is it in the gas ?
      IF(NUMED.EQ.5)THEN
         IF(DESTEP.GT.0.) THEN
            HITS(1)=DESTEP
            CALL GSCHIT(ISET,IDET,ITRA,NUMBV,HITS,1,IHIT)
         ENDIF
      ENDIF
*
*             Debug/plot event
      IF(IDEBUG.NE.0) THEN
         IF (ISWIT(2).EQ.2) CALL GPCXYZ
      ENDIF
*
*    BOOTSTRAP THE TRACKING PARTICLE.
      IF(LHBOOT(1).EQ.1.AND.ISTOP.EQ.0.AND.VECT(3).GT.-6.4) THEN
         IF(NUMED.EQ.3.OR.NUMED.EQ.6) THEN
            IF(IPART.EQ.2.OR.IPART.EQ.3.AND. GETOT.LE.EBTUP(1)) THEN
               CALL HBOOTG(1,0,IBOOT)
               IF(IBOOT.EQ.1) THEN
                  CALL HFILL(31,GETOT,0.,1.)
                  ISTOP=2
               ENDIF
            END IF
            IF(LHBOOT(2).EQ.1) THEN
               IF(IPART.GE.8.AND.IPART.LE.9.AND. GETOT.LE.EBTUP(2))
     +         THEN
                  CALL HBOOTG(2,0,IBOOT)
                  IF(IBOOT.EQ.1) THEN
                     CALL HFILL(41,GETOT,0.,1.)
                     ISTOP=2
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
      IF(IDEBUG.NE.0) THEN
         IF (ISWIT(2).EQ.2) CALL GPCXYZ
      ENDIF
*
      END
