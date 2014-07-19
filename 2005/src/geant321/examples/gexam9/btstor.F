*
* $Id: btstor.F,v 1.2 1996/02/22 13:32:37 ravndal Exp $
*
* $Log: btstor.F,v $
* Revision 1.2  1996/02/22 13:32:37  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:23  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE BTSTOR
************************************************************************
*
*   Collect the hits for
*     1) frozen file if ITHETA = 0
*     2) find the theta correlation of shower width and energy deposit
*
************************************************************************
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcbank.inc"
#include "geant321/gclist.inc"
#include "geant321/gcsets.inc"
#include "geant321/gcnum.inc"
#include "tstcom.inc"
#include "maphc.inc"
      PARAMETER  (NDDIM=2, NHDIM=1, NHMAX=100, NVDIM=4)
      INTEGER*2  NTOT,KDIGI(NDDIM,NHMAX)
      INTEGER  LITRA(1), LLAYF(NHMAX), LLAYZ(NHMAX)
     +        ,LTOWF(NHMAX), LTOWZ(NHMAX), LTRAF(NHMAX), LTRAZ(NHMAX)
     +        ,NUMBF(NVDIM,NHMAX), NUMBZ(NVDIM,NHMAX), NUMVS(NVDIM)
     +        ,MINUST(0:1)
      REAL  HITF(NHDIM,NHMAX), HITZ(NHDIM,NHMAX)
      CHARACTER*4  HBF, HBZ
      DATA  FACT /1.0E+08/, ORIG /0./, NTOW /9/, NLAY /10/
      DATA L0/3/,MINUSL/5/,NTUBE0/5/,MINUST/5,7/
*
* *** CHECK THAT DETECTOR SET 'HTST' EXISTS
*
      NSET = IQ(JSET - 1)
      CALL GLOOK('HTST',IQ(JSET+1),NSET,ISET)
      IF(ISET.LE.0) RETURN
      JS   = LQ(JSET-ISET)
*
      CALL VZERO (NUMVS, NVDIM)
      HBF = 'HBFC'
      HBZ = 'HBZC'
*
* *** Extract hits for each of the 3 relevant rings in turn
*
      CALL GFHITS ('HTST', HBF, NVDIM,NHDIM,NHMAX, 0, NUMVS,
     +               LTRAF, NUMBF, HITF, NHITF)
      CALL GFHITS ('HTST', HBZ, NVDIM,NHDIM,NHMAX, 0, NUMVS,
     +               LTRAZ, NUMBZ, HITZ, NHITZ)
      NTOT=NHITF+NHITZ
*
      CALL HFILL(24,1.*NTOT,0.,1.)
      IF(NTOT.GT.0) CALL HFILL(50,XYZ(3),0.0,1.)
      IF (NHITF.GT.NHMAX.OR.NHITZ.GT.NHMAX) THEN
         NHITF = MIN0(NHITF,NHMAX)
         NHITZ = MIN0(NHITZ,NHMAX)
         WRITE (6, 10000)
      END IF
      DO 10 I=1,NHITF
         IL=(NUMBF(3,I)+1)/2
         IT=NUMBF(4,I)
         KDIGI(1,I)=IL*100+IT
         EHIT=AMIN1(0.0002,HITF(1,I))
         KDIGI(2,I)=EHIT*FACT
         FMAP(IT,IL,ITHETA+1)=FMAP(IT,IL,ITHETA+1)+EHIT
   10 CONTINUE
      DO 20 I=1,NHITZ
         IL=NUMBZ(3,I)/2
         IT=NUMBZ(4,I)
         KDIGI(1,I+NHITF)=10000+IL*100+IT
         EHIT=AMIN1(0.0002,HITZ(1,I))
         KDIGI(2,I+NHITF)=EHIT*FACT
         ZMAP(IT,IL,ITHETA+1)=ZMAP(IT,IL,ITHETA+1)+EHIT
   20 CONTINUE
*
      IF(NTOT.EQ.0) THEN
         RNON(ITHETA+1)=RNON(ITHETA+1)+1
      ELSEIF(ITHETA.EQ.0) THEN
         KEVN=KEVN+1
         KP(KEVN)=NP1+1
         DO 30  I=1,NTOT
            IF(KDIGI(2,I).EQ.0.) GOTO 30
            LZ=KDIGI(1,I)/10000
            KKDIGI=KDIGI(1,I)
            LAYER=MOD(KKDIGI,10000)/100-MINUSL
            IF(LAYER.LT.0) LAYER=0
            IF(LAYER.GT.9) LAYER=9
            NTUBE=MOD(KKDIGI,100)-MINUST(LZ)
            IF(NTUBE.LT.0) NTUBE=0
            IBUF(NP1+2*I-1)=LZ*1000+LAYER*100+NTUBE
   30    IBUF(NP1+2*I)=KDIGI(2,I)
         NPZ=NP1+NTOT*2
         NP1 = NPZ
      ENDIF
*
10000 FORMAT (/,' BTSTOR: Hit buffer overflow, truncated to NHMAX')
*                                                             END BTSTOR
      END
