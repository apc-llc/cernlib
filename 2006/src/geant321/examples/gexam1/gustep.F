*
* $Id: gustep.F,v 1.2 1996/02/01 14:37:27 ravndal Exp $
*
* $Log: gustep.F,v $
* Revision 1.2  1996/02/01 14:37:27  ravndal
* Clean up of the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
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
#include "geant321/gcbank.inc"
#include "geant321/gctmed.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gcflag.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
#include "geant321/gccuts.inc"
#include "geant321/gconsp.inc"
#include "geant321/gcscan.inc"
#include "celoss.inc"
#include "pvolum.inc"
      DIMENSION NID(100)
      SAVE NID
      DATA NID/100*0/
C
#if !defined(CERNLIB_NEWFLUX)
      DATA NROLD / 1 /
#endif
 
      IF(SCANFL) THEN
         CALL GSCANU
      ELSE
C
C.
C              Something generated ?
 
         IF(NGKINE.GT.0) THEN
            DO 10 I=1,NGKINE
               ITYPA = GKIN(5,I)
               CALL GSKING(I)
               IF(ISWIT(9).NE.0)GO TO 10
#if defined(CERNLIB_CHECK)
               ID1=50+ITYPA
               CALL HFF1(ID1,NID(ID1),GKIN(4,I),1.)
               ID2=60+ITYPA
               CALL HFF1(ID2,NID(ID2),GKIN(4,I),1.)
#endif
   10       CONTINUE
         ENDIF
         IF(ISWIT(9).NE.0)GO TO 50
         IF(NUMED.EQ.2) THEN
            NRIN = NUMBER(NLEVEL)
 
C ***          Energy deposited
C
            IF(DESTEP.NE.0.)THEN
               NTUB = NUMBER(NLEVEL-1)
               DEDL(NRIN) = DEDL(NRIN) + DESTEP
               DEDR(NTUB) = DEDR(NTUB) + DESTEP
            ENDIF
C ***          Particle's flux
#if !defined(CERNLIB_NEWFLUX)
            IF(SLENG.LE.0.) NROLD = NRIN
            IF(NRIN.NE.NROLD) THEN
               NPL = (NRIN + NROLD)/2 + 1
               FNPAT(NPL,IPART) = FNPAT(NPL,IPART) + 1.
               NROLD = NRIN
            ENDIF
#endif
#if defined(CERNLIB_NEWFLUX)
            IF(INWVOL.EQ.1.AND.SLENG.GT.EPSIL)THEN
               DZ=20.*X0/FLOAT(NL)
               ZABS=VECT(3)+10.*X0
               ZABSM=MOD(ZABS,DZ)
               DELTA=0.5*EPSIL
               IF(ZABSM.GT.DZ-DELTA)THEN
                  NPL = NRIN+1
                  FNPAT(NPL,IPART) = FNPAT(NPL,IPART) + 1.
               ENDIF
               IF(ZABSM.LT.DELTA)THEN
                  NPL = NRIN
                  IF(NPL.EQ.0)NPL=1
                  FNPAT(NPL,IPART) = FNPAT(NPL,IPART) + 1.
               ENDIF
            ENDIF
#endif
 
#if defined(CERNLIB_CHECK)
 
C ***          Process calls frequency
C
            IF(NMEC.LE.0) GO TO 30
            DO 20 IM = 1,NMEC
               IF(LMEC(IM).GT.12) GO TO 20
               IDM = 30 + LMEC(IM)
               CALL HFF1(IDM,NID(IDM),GEKIN,1.)
   20       CONTINUE
   30       CONTINUE
 
#endif
C
C            If track is a photon extrapolate the track
C            directly to the next photon interaction
C            but still computing flux correctly
C
         ENDIF
 
#if defined(CERNLIB_CHECK)
 
C             Plot total track length
 
         IF(ISTOP.NE.0) THEN
            SLENRL = SLENG/X0
            ID3=20+IPART
            CALL HFF1(ID3,NID(ID3),SLENRL,1.)
         ENDIF
 
#endif
 
 
C             Debug event
 
  50     CALL GDEBUG
      END IF
 
      END
