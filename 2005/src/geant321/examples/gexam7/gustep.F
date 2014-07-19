*
* $Id: gustep.F,v 1.2 1996/02/22 13:24:03 ravndal Exp $
*
* $Log: gustep.F,v $
* Revision 1.2  1996/02/22 13:24:03  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:21  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
C.    *
C.    *       User routine called at the end of each tracking step
C.    *       INWVOL is different from 0 when the track has reached
C.    *              a volume boundary
C.    *       ISTOP is different from 0 if the track has stopped
C.    *
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gctmed.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
#include "celoss.inc"
#include "pvolum.inc"
#include "bootwk.inc"
      DIMENSION NID(100)
      DATA NID/100*0/
C
C ***         Debug event
C
      IF(IDEBUG*ISWIT(2).EQ.2) CALL GPCXYZ
C
C ***          Something generated ?
C
      IF(NGKINE.GT.0) CALL GSKING(0)
C
C ***          Energy deposited
C
       IF(DESTEP.NE.0.)THEN
          NL  = NUMBER(NLEVEL)
          NR  = NUMBER(NLEVEL-1)
          DEDL(NL) = DEDL(NL) + DESTEP
          DEDR(NR) = DEDR(NR) + DESTEP
       ENDIF
C
C ***     Leakage
C
      IF(INWVOL.EQ.3) THEN
         ZL = Z1 - EPSIL
         IF(VECT(3).LE.-ZL) THEN
            ELEAK(1) = ELEAK(1) + GETOT
         ELSEIF(VECT(3).GE.ZL) THEN
            ELEAK(2) = ELEAK(2) + GETOT
         ELSE
            ELEAK(3) = ELEAK(3) + GETOT
         ENDIF
      ENDIF
C
C ***          Particle's flux
C
       NL  = NUMBER(NLEVEL)
       IF(SLENG.LE.0.) NLOLD = NL
       IF(NL.NE.NLOLD) THEN
         NPL = (NL + NLOLD)/2 + 1
         IPA = MIN(IPART,4)
         FNPAT(NPL,IPA) = FNPAT(NPL,IPA) + 1.
         NLOLD = NL
       ENDIF
*
* *** Bootstrap for this particle ?
*
      IF(JBOOT.NE.0) CALL UGBOOT
C
#if defined(CERNLIB_CHECK)
C
C ***          Process calls frequency
C
       IF(NMEC.LE.0) GO TO 56
       DO 55 IM = 1,NMEC
       IF(LMEC(IM).GT.12) GO TO 55
       IDM = 30 + LMEC(IM)
       CALL HFF1(IDM,NID(IDM),GEKIN,1.)
   55  CONTINUE
   56  CONTINUE
C
C ***         Plot total track length
C
      IF(ISTOP.NE.0) THEN
          SLENRL = SLENG/X0
          ID3=20+MIN(IPART,4)
          CALL HFF1(ID3,NID(ID3),SLENRL,1.)
      ENDIF
C
C *** Energy of generated secondaries
C
      IF(NGKINE.GT.0) THEN
        DO 30 I=1,NGKINE
        ITYPA  = MIN(GKIN(5,I),4)
        EKING = GKIN(4,I)
        IF(EKING.LE.0.010) ID1=50+ITYPA
        IF(EKING.GT.0.010) ID1=60+ITYPA
        CALL HFF1(ID1,NID(ID1),GKIN(4,I),1.)
   30   CONTINUE
      ENDIF
 
#endif
 
      END
