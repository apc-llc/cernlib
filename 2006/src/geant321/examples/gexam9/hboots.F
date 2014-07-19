*
* $Id: hboots.F,v 1.2 1996/02/22 13:32:40 ravndal Exp $
*
* $Log: hboots.F,v $
* Revision 1.2  1996/02/22 13:32:40  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:24  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE HBOOTS
*
***********************************************************************
*   BOOTSTRAP ELECTRONS AND POSITIONS IN HACAL.
***********************************************************************
*
#include "hboot.inc"
#include "geant321/gcbank.inc"
#include "geant321/gcsets.inc"
#include "geant321/gcking.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
#include "geant321/gconst.inc"
#include "geant321/gctmed.inc"
      PARAMETER (LEBTIN=81,LPBTIN=82)
      PARAMETER (MAXENE=10,MAXEV=300)
      PARAMETER (MMP=2,MAXDAT=50000,MAXKP=MAXEV*MAXENE*1.5)
      PARAMETER (MAXL=8,NLOWE=2)
      INTEGER*2 NDATA(MAXDAT),KP(MAXKP)
      DIMENSION NPOI(MAXENE,MMP),NDPOI(MAXENE,MMP),RNON(10,MAXENE,MMP),
     +    AGLENE(MAXL,0:9,0:1,MAXENE,MMP),
     +    AGLWID(MAXL,0:9,0:1,MAXENE,MMP),
     +    ESIG(NLOWE,2),ARNON(NLOWE,2),ETUBE(NLOWE,2),
     +    VCELL(3),VBOOT(6),UCELL(3),ZCELL(3),KEVN(MAXENE,MMP)
      DATA ARNON/0.990,0.985,0.97,0.95/,DIFMAX/0.333333334/,
     +     ETUBE/1.3E-6,1.5E-6,0.039E-3,0.034E-3/,ESIG/4*1.0/,
     +     THRES/0.5/,THICKL/1.34/,WIDTH/1.06/
      DATA L0/3/,NTUBE0/15/
*
*    INITIALIZATION OF BOOTSTRAP IN HACAL.
      PAMASS(1) = EMASS
      PAMASS(2) = 0.139567
*
*          READ THE PREGENERATED SHOWERS AND THEIR ANGULAR COORELATION
      OPEN(UNIT=LEBTIN,FORM='UNFORMATTED',STATUS='OLD',
#if defined(CERNLIB_VAX)||defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
     +FILE='efr.dat')
#endif
#if defined(CERNLIB_IBM)
     +FILE='/EFR DAT *')
#endif
      READ(LEBTIN) NE(1),(EHBOOT(I,1),I=1,NE(1))
      WRITE(6,10000) NE(1),(EHBOOT(I,1),I=1,NE(1))
10000 FORMAT(//,' BOOTSTRAP ELECTRONS IN HACAL FOR',I2,
     +          ' KINETIC ENERGIES:',9F7.4)
      EBTUP(1)=1.3333334*EHBOOT(NE(1),1)+PAMASS(1)
      PRINT *,' UP LIMIT OF TOTAL ENERGY IS',EBTUP(1)
*
      JJ1=1
      LL1=1
      DO 10  N=1,NE(1)
         READ(LEBTIN) KEVN(N,1),(RNON(K,N,1),K=1,10),LENGTH,
     +        (((AGLENE(I1,I2,I3,N,1),I1=1,MAXL),I2=0,9),I3=0,1),
     +        (((AGLWID(I1,I2,I3,N,1),I1=1,MAXL),I2=0,9),I3=0,1)
         JJ2=JJ1+KEVN(N,1)-1
         NPOI(N,1)=JJ1
         NDPOI(N,1)=LL1
         READ(LEBTIN) (KP(I),I=JJ1,JJ2)
         LL2=LL1+LENGTH-1
         IF(LL2.GT.MAXDAT) THEN
            WRITE(6,10100)N,LL2,MAXDAT
10100       FORMAT(' Too much data during read ',3(1X,I10))
            NE(1)=N-1
            RETURN
         ENDIF
         READ(LEBTIN) (NDATA(I),I=LL1,LL2)
         WRITE(6,10200) EHBOOT(N,1),KEVN(N,1),LENGTH,JJ1,JJ2,LL1,LL2
         WRITE(6,10300) (RNON(K,N,1),K=1,10), (((AGLENE(I1,I2,I3,N,1),
     +   I1=1,MAXL),I2=0,9),I3=0,1), (((AGLWID(I1,I2,I3,N,1),I1=1,MAXL)
     +   ,I2=0,9),I3=0,1)
10200    FORMAT(/,' EBOOT =',F8.4,'   # of events',I5,5I8)
10300    FORMAT(/,' RNON',10F7.4,/,' AGLENE',8(/,20F6.2),
     +      /,' AGLWID',8(/,20F6.2))
         LL1=LL2+1
         JJ1=JJ2+1
   10 CONTINUE
*
      IF (LHBOOT(2) .EQ. 1) THEN
*
*   READ THE FROZEN FILE OF CHARGED PIONS
         OPEN(LPBTIN,FORM='UNFORMATTED',STATUS='OLD',
#if defined(CERNLIB_VAX)||defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
     +   FILE='efrp.dat')
#endif
#if defined(CERNLIB_IBM)
     +   FILE='/EFRP DAT *')
#endif
         READ(LPBTIN) NE(2),(EHBOOT(I,2),I=1,NE(2))
         WRITE(6,10400) NE(2),(EHBOOT(I,2),I=1,NE(2))
10400 FORMAT(//,' BOOTSTRAP PION IN HACAL FOR',I2,' ENERGIES:',9F7.4)
         EBTUP(2)=1.3333334*EHBOOT(NE(2),2)+PAMASS(2)
         PRINT *,' UP LIMIT OF TOTAL ENERGY IS',EBTUP(2)
         DO 20  N=1,NE(2)
            READ(LPBTIN) KEVN(N,2),(RNON(K,N,2),K=1,10),LENGTH, (((AGLE
     +      NE(I1,I2,I3,N,2),I1=1,MAXL),I2=0,9),I3=0,1), (((AGLWID(I1,
     +      I2,I3,N,2),I1=1,MAXL),I2=0,9),I3=0,1)
            JJ2=JJ1+KEVN(N,2)-1
            NPOI(N,2)=JJ1
            NDPOI(N,2)=LL1
            READ(LPBTIN) (KP(I),I=JJ1,JJ2)
            LL2=LL1+LENGTH-1
            IF(LL2.GT.MAXDAT) THEN
               PRINT *,' TOO NUCH DATA DURING READ  .',N,LL2,MAXDAT
               NE(2)=N-1
               RETURN
            ENDIF
            READ(LPBTIN) (NDATA(I),I=LL1,LL2)
            WRITE(6,10200) EHBOOT(N,2),KEVN(N,2),LENGTH,JJ1,JJ2,LL1,LL2
            WRITE(6,10300) (RNON(K,N,2),K=1,10), (((AGLENE(I1,I2,I3,N,
     +      2) ,I1=1,MAXL),I2=0,9),I3=0,1), (((AGLWID(I1,I2,I3,N,2),I1=
     +      1, MAXL),I2=0,9),I3=0,1)
            LL1=LL2+1
            JJ1=JJ2+1
   20    CONTINUE
      END IF
*
      RETURN
*
      ENTRY HBOOTG(MP,IPA,IBOOT)
*
*   BOOTSTRAP ELECTRONS AND POSITRONS IN HACAL.
*     INPUT MP =1 FOR ELECTRON ; 2 FOR CHARGED PION.
*           IPA=0  IF PRELIMINARY PARTICLE.
*           IPA>0 IF SECONDARY PARTICLE. ( = THE ORDER IN /GCKING/ )
*     OUTPUT IBOOT=0 IF IT WAS NOT BOOTSTRAPED
*            IBOOT=1 IF IT WAS BOOTSTRAPED
*
      IBOOT=0
      IFAIL=0
      LPRINT=0
      IF(IDEBUG.NE.0.AND.ISWIT(3).EQ.1) LPRINT=1
      JK=LQ(JKINE-ITRA)
*
*  BOOTSTRAP ONLY WHEN THE ENERGY OF PARTICLE < THRES*ENERGY OF
*  THE PRELIMINARY PARTICLE
*
      ETHRES=THRES*(Q(JK+4)-PAMASS(mp))
      IF(IPA.EQ.0) THEN
         EBOOT = GETOT-PAMASS(mp)
         IF(EBOOT.GT.ETHRES) GOTO 120
         DO 30  JDO=1,6
            VBOOT(JDO)=VECT(JDO)
   30    CONTINUE
      ELSEIF(IPA.GT.0) THEN
         EBOOT = GKIN(4,IPA)-PAMASS(mp)
         IF(EBOOT.GT.ETHRES) GOTO 120
         PMOM2 =GKIN(1,IPA)**2+GKIN(2,IPA)**2+GKIN(3,IPA)**2
         PMOINV = 1. / SQRT(PMOM2)
         DO 40  JDO=1,3
            VBOOT(JDO) = VECT(JDO)
            VBOOT(JDO+3) = PMOINV * GKIN(JDO,IPA)
   40    CONTINUE
      ELSE
         RETURN
      END IF
      IF(LPRINT.EQ.1) PRINT 10500,IPA,EBOOT,VBOOT
10500 FORMAT(' IPA,EBOOT,VBOOT',I3,7F9.3)
*
*  LET'S GO BOOTSTRAP
      IF(EBOOT.LE.EHBOOT(1,MP)*(1.-DIFMAX)) THEN
*
*  Energy < min. energy stored, deposit a part of the energy in
*        one tube next to the vertex only.
         IF(EBOOT.LE.0.0007) THEN
            GOTO 100
         ELSEIF(EBOOT.LE.0.002) THEN
            IEP=1
         ELSE
            IEP=2
         ENDIF
*
         RNN=RNDM(DUMMY)
         IF(RNN.LE.ARNON(IEP,MP)) GOTO 100
         CALL RANNOR(RNOR1,RNOR2)
         DEPENE = ETUBE(IEP,MP)*(1.0+ESIG(IEP,MP)*RNOR1)
         IF(DEPENE.LT.0.0) DEPENE = - DEPENE
*
         CALL FTUBES(0,VBOOT,VCELL,IFLAG,NEWMED)
         IF(IFLAG.EQ.0) GOTO 120
         ISVOL = 1
         CALL UCOPY(VCELL,VECT,3)
         CALL GFINDS
         CALL GSCHIT( ISET, IDET, ITRA, NUMBV, DEPENE, 1, IHIT )
         IF(IHIT.EQ.0) PRINT *,' GSCHIT ERROR'
         IF(LPRINT.eq.1) WRITE(6,10900) IPA, (NUMBV(NM),NM=1,5),DEPENE,
     +   EBOOT
         GO TO 90
      ENDIF
*
*  Energy > min. energy stored, take a shower and deposit cells contents
   50 CONTINUE
      DO 60  I=1,NE(MP)
         IRANGE=I
         IF(ABS(EBOOT-EHBOOT(I,MP))/EHBOOT(I,MP).LT.DIFMAX) GO TO 70
   60 CONTINUE
*
*   * Out of stored energy ranges  ==>  return
      GO TO 120
*
   70 CONTINUE
      SCALEF=EBOOT/EHBOOT(IRANGE,MP)
*
*      Energy values stored in frozen file are in unit of 10 eV
      SCALEF=SCALEF * .00000001
*
      CALL FTUBES(0,VBOOT,VCELL,IFLAG,NEWMED)
      IF(LPRINT.EQ.1) WRITE(6,10600) (VBOOT(I),I=1,3),VCELL,NEWMED
10600 FORMAT(' VBOOT,VCELL,NEWMED',6F9.3,I5)
      IF(IFLAG.EQ.0) GOTO 120
      ISVOL = 1
      CALL UCOPY(VCELL,VECT,3)
      CALL GFINDS
      IPL=NUMBV(3)
      IF(IPL.EQ.1) GOTO 110
      LPHI=MOD(IPL,2)
      CALL UCOPY(VCELL,ZCELL,3)
*
*   ANGLE = THETA ANGLE W.R.T. BEAM AXIS.
      ANGLE=ACOS(VBOOT(6))
      IANG=(ANGLE*RADDEG+5.)/10.0
      DIREC=1.0
      IF(IANG.GT.9) THEN
         IANG=18-IANG
         DIREC=-1.0
      ENDIF
      IF(RNDM(DUMMY).LE.RNON(IANG,IRANGE,MP)) GOTO 100
      ANGPHI=ATAN2(VBOOT(5),VBOOT(4))
*
      KSH=RNDM(DUMMY)*KEVN(IRANGE,MP)
      KPP=NPOI(IRANGE,MP)+KSH
*
*  THE SELECTED SHOWER IS IN NDATA(KSH1) - NDATA(KSH2)
*
      KSH1=KP(KPP)+NDPOI(IRANGE,MP)-1
      KSH2=KP(KPP+1)+NDPOI(IRANGE,MP)-3
*
      DO 80  LK=KSH1,KSH2,2
         ID=NDATA(LK)
         IF(ID.EQ.0.OR.NDATA(LK+1).EQ.0) GOTO 80
         LZ=ID/1000
         IL=MOD(ID,1000)/100-L0
         IT=MOD(ID,100)
*         CALL UCOPY(ZCELL,UCELL,3)
         IF(LPHI.EQ.1.AND.LZ.EQ.0) THEN
            LSHIFT=IL*cos(ANGLE)
            LSHIFT=LSHIFT*2
            II=1
         ELSEIF(LPHI.EQ.1.AND.LZ.EQ.1) THEN
            LSHIFT=IL*cos(ANGLE)
            LSHIFT=LSHIFT*2+ 1
            II=2
         ELSEIF(LPHI.EQ.0.AND.LZ.EQ.1) THEN
            LSHIFT=IL*cos(ANGLE)
            LSHIFT=LSHIFT*2 + 1
            II=1
         ELSE
            LSHIFT=IL*cos(ANGLE)
            LSHIFT=LSHIFT*2
            II=2
         ENDIF
*
         ILL=MIN0(IL,6)
         IF(ILL.LT.-1) ILL=-1
         AGLW = AGLWID(ILL+2,IANG,LPHI,IRANGE,MP)
         AGLE = AGLENE(ILL+2,IANG,LPHI,IRANGE,MP)
         IF (AGLE.EQ.0.0) AGLE = 1.
         SHIFT0=THICKL*LSHIFT*SIN(ANGLE)
         RNTUBE=IT-NTUBE0
         IF(LZ.EQ.0) RNTUBE=RNTUBE-ILL*0.5
         SHIFT=SHIFT0+WIDTH*RNTUBE*AGLW
         UCELL(1)=ZCELL(1)+SHIFT*COS(ANGPHI)
         UCELL(2)=ZCELL(2)+SHIFT*SIN(ANGPHI)
         UCELL(3)=ZCELL(3)+THICKL*LSHIFT
         CALL FTUBES(II,UCELL,VCELL,IFLAG,NEWMED)
         IF(LPRINT.EQ.1) WRITE(6,10700) ID,IL,IT,UCELL,VCELL
10700    FORMAT(' ID,IL,IT,UCELL,VCELL',3I5,6F9.3)
         IF(IFLAG.EQ.0) THEN
            IFAIL=IFAIL+1
            IF(LPRINT.EQ.0.AND.IFAIL.LE.100) WRITE(6,10800) ID,
     +      NDATA(LK+1),NEWMED,ZCELL,UCELL,VCELL
10800    FORMAT(' FAIL: ID,NEWMED,Z-U-VCELL',I5,I9,I2,9F7.2)
            GOTO 80
         ENDIF
         DEPENE=SCALEF*NDATA(LK+1)*AGLE
         IF(DEPENE.LT.0.0) DEPENE = DEPENE
         CALL UCOPY(VCELL,VECT,3)
         CALL GFINDS
         IF(LPRINT.EQ.1)WRITE(6,10900)IPA,(NUMBV(M),M=1,5),DEPENE,EBOOT
10900    FORMAT(' BOOTSTRAP AT',6I4,2X,E11.5,F9.5)
         CALL GSCHIT( ISET, IDET, ITRA, NUMBV, DEPENE, 1, IHIT )
         IF(IHIT.EQ.0) PRINT *,' GSCHIT ERROR'
   80 CONTINUE
*
   90 ISVOL = 0
      CALL UCOPY(VBOOT,VECT,3)
      IF(LPRINT.EQ.1.AND.IFAIL.NE.0) PRINT *,' IFAIL',IFAIL
  100 IBOOT=1
      CALL HFILL(22+MP*10,VBOOT(6),0.,1.)
      RETURN
*
  110 ISVOL = 0
      CALL UCOPY(VBOOT,VECT,3)
      IF(LPRINT.EQ.1) PRINT *,' NO BOTTSTRAP IN THE FIRST PLANE'
  120 RETURN
      END
