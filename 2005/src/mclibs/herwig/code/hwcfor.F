CDECK  ID>, HWCFOR.
*CMZ :-        -26/04/91  14.15.56  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWCFOR
C-----------------------------------------------------------------------
C     Converts colour-connected quark-antiquark pairs into clusters
C     Modified by IGK to include BRW's colour rearrangement and
C     MHS's cluster vertices
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWULDO,HWVDOT,HWRGEN,HWUPCM,DCL0,DCL(4),DCL1,
     & DFAC,DISP1(4),DISP2(4),DMAX,PCL(5),DOT1,DOT2,FAC,VCLUS,SCA1,SCA2,
     & EM0,EM1,EM2,PC0,PC1
      INTEGER HWRINT,MAP(120),NREST1,NREST2,IBHEP,IBCL,NRECO,JBHEP,JHEP,
     & KHEP,LHEP,LCL,IHEP,MCL,I,ISTJ,ISTK,JCL,ID1,ID3,L
      LOGICAL HWRLOG,QORQQB(NMXRES),QBORQQ(NMXRES),SPLIT
      EXTERNAL HWULDO,HWVDOT,HWRGEN,HWUPCM,HWRINT
      PARAMETER (NREST1=NMXRES-120,NREST2=NMXRES-114)
      COMMON/HWCFRM/VCLUS(4,NMXHEP)
      DATA MAP/1,2,3,4,5,6,1,2,3,4,5,6,96*0,7,8,9,10,11,12,7,8,9,10,11,
     & 12/
      DATA QORQQB/6*.TRUE.,108*.FALSE.,6*.TRUE.,NREST1*.FALSE./
      DATA QBORQQ/6*.FALSE.,6*.TRUE.,96*.FALSE.,6*.TRUE.,NREST2*.FALSE./
      IF (IERROR.NE.0) RETURN
C Split gluons
      CALL HWCGSP
C Find colour partners after baryon number violating event
      IF (HVFCEN) CALL HVCBVI
      IF (IERROR.NE.0) RETURN
C Look for partons to cluster
      DO 10 IBHEP=1,NHEP
  10  IF (ISTHEP(IBHEP).GE.150.AND.ISTHEP(IBHEP).LE.154) GOTO 20
      IBCL=1
      GOTO 130
C Allow for clour rearrangement of primary clusters
  20  IF (CLRECO) THEN
        NRECO=0
C Randomize starting point
        JBHEP=HWRINT(IBHEP,NHEP)
        JHEP=JBHEP
  30    JHEP=JHEP+1
        IF (JHEP.GT.NHEP) JHEP=IBHEP
        IF (ISTHEP(JHEP).GE.150.AND.ISTHEP(JHEP).LE.154.AND.
     &      QORQQB(IDHW(JHEP))) THEN
C Find colour connected antiquark or diquark
          KHEP=JMOHEP(2,JHEP)
C Find partner antiquark or diquark
          LHEP=JDAHEP(2,JHEP)
C Find closest antiquark or diquark
          DCL0=1.D15
          LCL=0
          DO 40 IHEP=IBHEP,NHEP
          IF (ISTHEP(IHEP).GE.150.AND.ISTHEP(IHEP).LE.154.AND.
     &        QBORQQ(IDHW(IHEP))) THEN
C Check whether already reconnected
            IF (JDAHEP(2,IHEP).GT.0.AND.IHEP.NE.LHEP) THEN
              CALL HWVDIF(4,VHEP(1,IHEP),VHEP(1,JHEP),DCL)
              DCL1=ABS(HWULDO(DCL,DCL))
              IF (DCL1.LT.DCL0) THEN
                DCL0=DCL1
                LCL=IHEP
              ENDIF
            ENDIF
          ENDIF
  40      CONTINUE
          IF (LCL.NE.0.AND.LCL.NE.KHEP) THEN
            MCL=JDAHEP(2,LCL)
            IF (JDAHEP(2,MCL).NE.KHEP) THEN
C Pairwise reconnection is possible
              CALL HWVDIF(4,VHEP(1,KHEP),VHEP(1,MCL ),DCL)
              DCL0=DCL0+ABS(HWULDO(DCL,DCL))
              CALL HWVDIF(4,VHEP(1,JHEP),VHEP(1,KHEP),DCL)
              DCL1=ABS(HWULDO(DCL,DCL))
              CALL HWVDIF(4,VHEP(1,LCL ),VHEP(1,MCL ),DCL)
              DCL1=DCL1+ABS(HWULDO(DCL,DCL))
              IF (DCL0.LT.DCL1.AND.HWRLOG(PRECO)) THEN
C Reconnection occurs
                JMOHEP(2,JHEP)= LCL
                JDAHEP(2,LCL )=-JHEP
                JMOHEP(2,MCL) = KHEP
                JDAHEP(2,KHEP)=-MCL
                NRECO=NRECO+1
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF (JHEP.NE.JBHEP) GOTO 30
        IF (NRECO.NE.0) THEN
          DO 50 IHEP=IBHEP,NHEP
  50      JDAHEP(2,IHEP)=ABS(JDAHEP(2,IHEP))
        ENDIF
      ENDIF
C Find (adjusted) cluster positions using MHS prescription
      DFAC=10
      DMAX=1D-10
      DO 70 JHEP=IBHEP,NHEP
      IF (ISTHEP(JHEP).GE.150.AND.ISTHEP(JHEP).LE.154.AND.
     &    QORQQB(IDHW(JHEP))) THEN
        KHEP=JMOHEP(2,JHEP)
        CALL HWUDKL(IDHW(JHEP),PHEP(1,JHEP),DISP1)
        CALL HWVSCA(4,DFAC,DISP1,DISP1)
        CALL HWUDKL(IDHW(KHEP),PHEP(1,KHEP),DISP2)
        CALL HWVSCA(4,DFAC,DISP2,DISP2)
C Rescale the lengths of DISP1,DISP2 if too long
        DOT1=HWVDOT(3,DISP1,DISP1)
        DOT2=HWVDOT(3,DISP2,DISP2)
        IF (MAX(DOT1,DOT2).GT.DMAX**2) THEN
          CALL HWVSCA(4,DMAX/SQRT(DOT1),DISP1,DISP1)
          CALL HWVSCA(4,DMAX/SQRT(DOT2),DISP2,DISP2)
        ENDIF
        CALL HWVSUM(4,PHEP(1,JHEP),PHEP(1,KHEP),PCL)
        DOT1=HWVDOT(3,DISP1,PCL)
        DOT2=HWVDOT(3,DISP2,PCL)
C If PCL > 90^o from either quark, use a vector which isn't
        IF (DOT1.LE.0..OR. DOT2.LE.0.) THEN
          CALL HWVSUM(4,DISP1,DISP2,PCL)
          DOT1=HWVDOT(3,DISP1,PCL)
          DOT2=HWVDOT(3,DISP2,PCL)
        ENDIF
C If vectors are exactly opposite each other this method cannot work
        IF (DOT1.EQ.0.OR.DOT2.EQ.0) THEN
C So use midpoint of quark constituents
          CALL HWVSUM(4,VHEP(1,JHEP),VHEP(1,KHEP),VCLUS(1,JHEP))
          CALL HWVSCA(4,0.5D0,VCLUS(1,JHEP),VCLUS(1,JHEP))
          GOTO 70
        ENDIF
C Rescale DISP1 or DISP2 to give equal components in the PCL direction
        FAC=DOT1/DOT2
        IF (FAC.GT.1) THEN
          CALL HWVSCA(4,    FAC,DISP2,DISP2)
          DOT2=DOT1
        ELSE
          CALL HWVSCA(4,ONE/FAC,DISP1,DISP1)
          DOT1=DOT2
        ENDIF
C Shift VHEP(1,JHEP) or VHEP(1,KHEP) s.t. their line is perp to PCL
        FAC=(HWVDOT(3,PCL,VHEP(1,KHEP))
     &      -HWVDOT(3,PCL,VHEP(1,JHEP)))/DOT1
        SCA1=MAX(ONE,ONE+FAC)
        SCA2=MAX(ONE,ONE-FAC)
        DO 60 I=1,4
  60    VCLUS(I,JHEP)=.5*(VHEP(I,JHEP)+VHEP(I,KHEP)
     &                   +SCA1*DISP1(I)+SCA2*DISP2(I))
      ENDIF
  70  CONTINUE
C First chop up beam/target clusters
      DO 80 JHEP=IBHEP,NHEP
      KHEP=JMOHEP(2,JHEP)
      ISTJ=ISTHEP(JHEP)
      ISTK=ISTHEP(KHEP)
      IF (QORQQB(IDHW(JHEP)).AND.
     &   (((ISTJ.EQ.153.OR.ISTJ.EQ.154).AND.ISTK.NE.151).OR.
     &    ((ISTK.EQ.153.OR.ISTK.EQ.154).AND.ISTJ.NE.151))) THEN
        CALL HWVSUM(4,PHEP(1,JHEP),PHEP(1,KHEP),PCL)
        CALL HWUMAS(PCL)
        CALL HWCCUT(JHEP,KHEP,PCL,.TRUE.,SPLIT)
      ENDIF
  80  CONTINUE
C Second chop up massive pairs
      DO 100 JHEP=IBHEP,NMXHEP
      IF (JHEP.GT.NHEP) GOTO 110
      IF (ISTHEP(JHEP).GE.150.AND.ISTHEP(JHEP).LE.154.AND.
     &    QORQQB(IDHW(JHEP))) THEN
  90    KHEP=JMOHEP(2,JHEP)
        CALL HWVSUM(4,PHEP(1,JHEP),PHEP(1,KHEP),PCL)
        CALL HWUMAS(PCL)
        IF (PCL(5).GT.CTHRPW(MAP(IDHW(JHEP)),MAP(IDHW(KHEP)))) THEN
          CALL HWCCUT(JHEP,KHEP,PCL,.FALSE.,SPLIT)
          IF (SPLIT) GOTO 90
        ENDIF
      ENDIF
  100 CONTINUE
C Third create clusters and store production vertex
  110 IBCL=NHEP+1
      JCL=NHEP
      DO 120 JHEP=IBHEP,NHEP
      IF (ISTHEP(JHEP).GE.150.AND.ISTHEP(JHEP).LE.154.AND.
     &    QORQQB(IDHW(JHEP))) THEN
        JCL=JCL+1
        IDHW(JCL)=19
        IDHEP(JCL)=91
        KHEP=JMOHEP(2,JHEP)
        CALL HWVSUM(4,PHEP(1,JHEP),PHEP(1,KHEP),PHEP(1,JCL))
        CALL HWUMAS(PHEP(1,JCL))
        IF (ISTHEP(JHEP).EQ.153.OR.ISTHEP(KHEP).EQ.153) THEN
          ISTHEP(JCL)=164
        ELSEIF (ISTHEP(JHEP).EQ.154.OR.ISTHEP(KHEP).EQ.154) THEN
          ISTHEP(JCL)=165
        ELSE
          ISTHEP(JCL)=163
        ENDIF
        JMOHEP(1,JCL)=JHEP
        JMOHEP(2,JCL)=KHEP
        JDAHEP(1,JCL)=0
        JDAHEP(2,JCL)=0
        JDAHEP(1,JHEP)=JCL
        JDAHEP(1,KHEP)=JCL
        ISTHEP(JHEP)=ISTHEP(JHEP)+8
        ISTHEP(KHEP)=ISTHEP(KHEP)+8
        CALL HWVEQU(4,VCLUS(1,JHEP),VHEP(1,JCL))
      ENDIF
  120 CONTINUE
      NHEP=JCL
C Fix up momenta for single-hadron clusters
  130 DO 150 JCL=IBCL,NHEP
C Don't hadronize beam/target clusters
      IF (ISTHEP(JCL).LT.163.OR.ISTHEP(JCL).GT.165) GOTO 150
      IF (ISTHEP(JCL).NE.163.AND.GENSOF) GOTO 150
C Set up flavours for cluster decay
      CALL HWCFLA(IDHW(JMOHEP(1,JCL)),IDHW(JMOHEP(2,JCL)),ID1,ID3)
      EM0=PHEP(5,JCL)
      IF ((B1LIM.EQ.ZERO).OR.(ID1.NE.11.AND.ID3.NE.11)) THEN
        IF (EM0.GT.RMIN(ID1,2)+RMIN(2,ID3)) GOTO 150
      ELSE
C Special for b clusters: allow 1-hadron decay above threshold
        IF (B1LIM*HWRGEN(1).GT.EM0/(RMIN(ID1,2)+RMIN(2,ID3))-1.)
     &   GOTO 150
      ENDIF
      EM1=RMIN(ID1,ID3)
      IF (ABS(EM0-EM1).LT.1.D-3) GOTO 150
C Decide to go backward or forward to transfer 4-momentum
      L=1-TWO*INT(HALF+HWRGEN(2))
      MCL=NHEP-IBCL+1
      LCL=JCL
      DO 140 I=1,MCL
      LCL=LCL+L
      IF (LCL.LT.IBCL) LCL=LCL+MCL
      IF (LCL.GT.NHEP) LCL=LCL-MCL
      IF (LCL.EQ.JCL) THEN
        IF (EM0.GE.EM1+RMIN(1,1)) GOTO 150
        CALL HWWARN('HWCFOR',101,*999)
      ENDIF
      IF (ISTHEP(LCL).LT.163.OR.ISTHEP(LCL).GT.165) GOTO 140
C Rescale momenta in 2-cluster CoM
      CALL HWVSUM(4,PHEP(1,JCL),PHEP(1,LCL),PCL)
      CALL HWUMAS(PCL)
      EM2=PHEP(5,LCL)
      PC0=HWUPCM(PCL(5),EM0,EM2)
      PC1=HWUPCM(PCL(5),EM1,EM2)
      IF (PC1.LT.0.) THEN
C Need to rescale other mass as well
        CALL HWCFLA(IDHW(JMOHEP(1,LCL)),IDHW(JMOHEP(2,LCL)),ID1,ID3)
        EM2=RMIN(ID1,ID3)
        PC1=HWUPCM(PCL(5),EM1,EM2)
        IF (PC1.LT.0.) GOTO 140
        PHEP(5,LCL)=EM2
      ENDIF
      IF (PC0.GT.0.) THEN
        PC0=PC1/PC0
        CALL HWULOF(PCL,PHEP(1,JCL),PHEP(1,JCL))
        CALL HWVSCA(3,PC0,PHEP(1,JCL),PHEP(1,JCL))
        PHEP(4,JCL)=SQRT(PC1**2+EM1**2)
        PHEP(5,JCL)=EM1
        CALL HWULOB(PCL,PHEP(1,JCL),PHEP(1,JCL))
        CALL HWVDIF(4,PCL,PHEP(1,JCL),PHEP(1,LCL))
        GOTO 150
      ELSEIF (PC0.EQ.0.) THEN
        PHEP(5,JCL)=EM1
        CALL HWDTWO(PCL,PHEP(1,JCL),PHEP(1,LCL),PC1,TWO,.TRUE.)
        GOTO 150
      ELSE
        CALL HWWARN('HWCFOR',102,*999)
      ENDIF
  140 CONTINUE
      CALL HWWARN('HWCFOR',103,*999)
  150 CONTINUE
      ISTAT=60
C Non-partons labelled as partons (ie photons) should get copied
      DO 160 IHEP=1,NHEP
      IF (ISTHEP(IHEP).EQ.150) THEN
        NHEP=NHEP+1
        JDAHEP(1,IHEP)=NHEP
        ISTHEP(IHEP)=157
        ISTHEP(NHEP)=190
        IDHW(NHEP)=IDHW(IHEP)
        IDHEP(NHEP)=IDPDG(IDHW(IHEP))
        CALL HWVEQU(5,PHEP(1,IHEP),PHEP(1,NHEP))
        CALL HWVEQU(4,VHEP(1,IHEP),VHEP(1,NHEP))
        JMOHEP(1,NHEP)=IHEP
        JMOHEP(2,NHEP)=JMOHEP(1,IHEP)
        JDAHEP(1,NHEP)=0
        JDAHEP(2,NHEP)=0
      ENDIF
  160 CONTINUE
  999 END
