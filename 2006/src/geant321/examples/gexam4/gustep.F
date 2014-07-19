*
* $Id: gustep.F,v 1.2 1996/02/01 15:00:36 ravndal Exp $
*
* $Log: gustep.F,v $
* Revision 1.2  1996/02/01 15:00:36  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:14  cernlib
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
#include "geant321/gcbank.inc"
#include "geant321/gctmed.inc"
#include "geant321/gcking.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
#include "urmips.inc"
      DIMENSION X(13)
      SAVE KMOD2,KMOD3,N500,N5,N6,XMIPMA,TOMAX,IPLOLD,PLMIP,SSTEP,FIRST
      CHARACTER*4 CMOD2, CMOD3
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA CMOD2, CMOD3 / 'MOD2', 'MOD3' /
      DATA N500/0/
      DATA N5/0/
      DATA N6/0/
      DATA XMIPMA/70./
      DATA TOMAX/2.E-7/
*
*     -----------------------------------------------------------------
*
      IF(FIRST) THEN
         CALL UCTOH(CMOD2, KMOD2, 4, 4)
         CALL UCTOH(CMOD3, KMOD3, 4, 4)
         FIRST = .FALSE.
      ENDIF
      IF(SLENG.LE.0.)THEN
         IPLOLD=0
         PLMIP=0.
         SSTEP=0.
      ENDIF
*             Something generated ?
      IF(NGKINE.GT.0) THEN
        DO 5 I=1,NGKINE
          ITYPA  = GKIN(5,I)
          UPWGHT = ITYPA
          IF(ITYPA.NE.4)  CALL GSKING(I)
   5    CONTINUE
      ENDIF
*
*             Are we in the gas ?
*
      IF(NUMED.EQ.5)THEN
*
*             Evaluate average energy loss for muons
*
         IF(ITRTYP.EQ.5)THEN
            IF(DESTEP.GT.0..AND.GETOT.GT.0.3)THEN
               IF(STEP.GT.0.1)THEN
                  DEDX=DESTEP/STEP
                  CALL HFF1(500,N500,DEDX,1.)
               ENDIF
            ENDIF
         ENDIF
*
*             Compute number of MIPS
*
         IPL=NUMBER(3)
         IF(NAMES(3).EQ.KMOD2)IPL=IPL+64
         IF(NAMES(3).EQ.KMOD3)IPL=IPL+99
         IF(DESTEP.GT.0.)THEN
            IF(IPL.LE.64)THEN
               DXMIP=DESTEP/CALIB1
            ELSE
               DXMIP=DESTEP/CALIB2
            ENDIF
            IF(IPL.EQ.IPLOLD)THEN
               PLMIP=PLMIP+DXMIP
               SSTEP=SSTEP+STEP
            ELSE
               PLMIP=DXMIP
               SSTEP=STEP
               IPLOLD=IPL
            ENDIF
#if defined(CERNLIB_NTUPLE)
            X(1)=IEVENT
            X(2)=IPART
            X(3)=GEKIN
            X(4)=ISTOP
            X(5)=INWVOL
            X(6)=STEP
            X(7)=DESTEP
            X(8)=DXMIP
            X(9)=TOFG
            X(10)=IPL
            X(11)=PLMIP
            X(12)=SSTEP
            X(13)=0
            DO 10 JMEC=1,NMEC
               IF(LMEC(JMEC).EQ.109) X(13)=1
  10        CONTINUE
            CALL HFN(1,X)
#endif
            CALL HFF1(5,N5,DXMIP,1.)
            CALL HFF1(6,N6,DXMIP,1.)
            IF(PLMIP.LT.XMIPMA.AND.TOFG.LT.TOMAX)THEN
               XMIP=XMIP+DXMIP
            ENDIF
         ENDIF
*
*             Count number of particles crossing planes
*
         IF(INWVOL.EQ.1)THEN
            IF(MOD(IPL,2).EQ.0)THEN
               KPL=IPL/2
               XKPL(KPL,ITRTYP)=XKPL(KPL,ITRTYP)+1.
            ENDIF
         ENDIF
      ENDIF
*
*             Debug/plot event
*
      CALL GDEBUG
*     IF(ISTOP.NE.0) THEN
*        IF(IPART.EQ.13) THEN
*          CALL GDTRAK('R')
*        ELSE
*          CALL GDTRAK('DR')
*        ENDIF
*     ENDIF
*
      END
