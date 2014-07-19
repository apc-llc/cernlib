CDECK  ID>, HWCCUT.
*CMZ :-        -26/04/91  14.29.39  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWCCUT(JHEP,KHEP,PCL,BTCLUS,SPLIT)
C-----------------------------------------------------------------------
C     Cuts into 2 the cluster, momentum PCL, made of partons JHEP & KHEP
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWREXQ,HWUPCM,HWRGEN,HWVDOT,EMC,QM1,QM2,EMX,EMY,
     & QM3,PXY,PCX,PCY,RCM,PCL(5),AX(5),PA(5),PB(5),PC(5),SKAPPA,DELTM,
     & VSCA,VTMP(4),RKAPPA,VCLUS
      INTEGER HWRINT,JHEP,KHEP,LHEP,MHEP,ID1,ID2,ID3,NTRY,NTRYMX,J
      LOGICAL BTCLUS,SPLIT
      EXTERNAL HWREXQ,HWUPCM,HWRGEN,HWVDOT,HWRINT
      COMMON/HWCFRM/VCLUS(4,NMXHEP)
      PARAMETER (SKAPPA=1.,NTRYMX=100)
      IF (IERROR.NE.0) RETURN
      EMC=PCL(5)
      ID1=IDHW(JHEP)
      ID2=IDHW(KHEP)
      QM1=RMASS(ID1)
      QM2=RMASS(ID2)
      SPLIT=.FALSE.
      NTRY=0
      IF (BTCLUS) THEN
C Split beam and target clusters as soft clusters
C Both (remnant) children treated like soft clusters if IOPREM=0(1)
  10    ID3=HWRINT(1,2)
        QM3=RMASS(ID3)
        IF (EMC.LE.QM1+QM2+2.*QM3) THEN
          ID3=3-ID3
          QM3=RMASS(ID3)
          IF (EMC.LE.QM1+QM2+2.*QM3) RETURN
        ENDIF
        PXY=EMC-QM1-QM2-TWO*QM3
        IF (ISTHEP(JHEP).EQ.153.OR.ISTHEP(JHEP).EQ.154.OR.
     &      IOPREM.EQ.0) THEN
          EMX=QM1+QM3+HWREXQ(BTCLM,PXY)
        ELSE
          EMX=QM1+QM3+PXY*HWRGEN(0)**PSPLT
        ENDIF
        IF (ISTHEP(KHEP).EQ.153.OR.ISTHEP(KHEP).EQ.154.OR.
     &      IOPREM.EQ.0) THEN
          EMY=QM2+QM3+HWREXQ(BTCLM,PXY)
        ELSE
          EMY=QM2+QM3+PXY*HWRGEN(1)**PSPLT
        ENDIF
        IF (EMX+EMY.GE.EMC) THEN
          NTRY=NTRY+1
          IF (NTRY.GT.NTRYMX) RETURN
          GOTO 10
        ENDIF
        PCX=HWUPCM(EMX,QM1,QM3)
        PCY=HWUPCM(EMY,QM2,QM3)
      ELSE
C Choose fragment masses for ordinary cluster
        PXY=EMC-QM1-QM2
  20    NTRY=NTRY+1
        IF (NTRY.GT.NTRYMX) RETURN
  30    EMX=QM1+PXY*HWRGEN(0)**PSPLT
        EMY=QM2+PXY*HWRGEN(1)**PSPLT
        IF (EMX+EMY.GE.EMC) GOTO 30
C u,d,s pair production with weights QWT
  40    ID3=HWRINT(1,3)
        IF (QWT(ID3).LT.HWRGEN(3)) GOTO 40
        QM3=RMASS(ID3)
        PCX=HWUPCM(EMX,QM1,QM3)
        IF (PCX.LT.0.) GOTO 20
        PCY=HWUPCM(EMY,QM2,QM3)
        IF (PCY.LT.0.) GOTO 20
        SPLIT=.TRUE.
      ENDIF
C Boost antiquark to CoM frame to find axis
      CALL HWULOF(PCL,PHEP(1,KHEP),AX)
      RCM=1./SQRT(HWVDOT(3,AX,AX))
      CALL HWVSCA(3,RCM,AX,AX)
C Construct new CoM momenta (collinear)
      PXY=HWUPCM(EMC,EMX,EMY)
      CALL HWVSCA(3,PXY,AX,PC)
      PC(4)=SQRT(PXY**2+EMY**2)
      PC(5)=EMY
      CALL HWVSCA(3,PCY,AX,PA)
      PA(4)=SQRT(PCY**2+QM2**2)
      PA(5)=QM2
      CALL HWULOB(PC,PA,PB)
      CALL HWVDIF(4,PC,PB,PA)
      PA(5)=QM3
      LHEP=NHEP+1
      MHEP=NHEP+2
      CALL HWULOB(PCL,PB,PHEP(1,KHEP))
      CALL HWULOB(PCL,PA,PHEP(1,MHEP))
      CALL HWVSCA(3,-ONE,PC,PC)
      PC(4)=EMC-PC(4)
      PC(5)=EMX
      CALL HWVSCA(3,PCX,AX,PA)
      PA(4)=SQRT(PCX**2+QM3**2)
      CALL HWULOB(PC,PA,PB)
      CALL HWULOB(PCL,PB,PHEP(1,LHEP))
      DO 50 J=1,4
  50  PHEP(J,JHEP)=PCL(J)-PHEP(J,KHEP)-PHEP(J,LHEP)-PHEP(J,MHEP)
      PHEP(5,JHEP)=QM1
      CALL HWVEQU(4,VHEP(1,LHEP),VHEP(1,MHEP))
C Construct new vertex positions
      RKAPPA=GEV2MM/SKAPPA
      CALL HWVSCA(3,RKAPPA,AX,AX)
      DELTM=(EMX-EMY)*(EMX+EMY)/(TWO*EMC)
      CALL HWVSCA(3,DELTM,AX,VTMP)
      VTMP(4)=(HALF*EMC-PXY)*RKAPPA
      CALL HWULB4(PCL,VTMP,VTMP)
      CALL HWVSUM(4,VTMP,VCLUS(1,JHEP),VHEP(1,LHEP))
      CALL HWVEQU(4,VHEP(1,LHEP),VHEP(1,MHEP))
      VSCA=0.25*EMC+HALF*(PXY+DELTM)
      CALL HWVSCA(3,VSCA,AX,VTMP)
      VTMP(4)=(EMC-VSCA)*RKAPPA
      CALL HWULB4(PCL,VTMP,VTMP)
      CALL HWVSUM(4,VTMP,VCLUS(1,JHEP),VCLUS(1,MHEP))
      VSCA=-0.25*EMC+HALF*(DELTM-PXY)
      CALL HWVSCA(3,VSCA,AX,VTMP)
      VTMP(4)=(EMC+VSCA)*RKAPPA
      CALL HWULB4(PCL,VTMP,VTMP)
      CALL HWVSUM(4,VTMP,VCLUS(1,JHEP),VCLUS(1,JHEP))
C (Re-)label quarks
      IDHW(LHEP)=ID3+6
      IDHW(MHEP)=ID3
      IDHEP(MHEP)= IDPDG(ID3)
      IDHEP(LHEP)=-IDPDG(ID3)
      ISTHEP(LHEP)=151
      ISTHEP(MHEP)=151
      JMOHEP(2,JHEP)=LHEP
      JDAHEP(2,KHEP)=MHEP
      JMOHEP(1,LHEP)=JMOHEP(1,KHEP)
      JMOHEP(2,LHEP)=MHEP
      JDAHEP(1,LHEP)=0
      JDAHEP(2,LHEP)=JHEP
      JMOHEP(1,MHEP)=JMOHEP(1,JHEP)
      JMOHEP(2,MHEP)=KHEP
      JDAHEP(1,MHEP)=0
      JDAHEP(2,MHEP)=LHEP
      NHEP=NHEP+2
  999 END
