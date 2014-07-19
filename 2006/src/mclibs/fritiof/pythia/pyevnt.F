*
* $Id: pyevnt.F,v 1.1.1.1 1996/01/11 14:05:26 mclareni Exp $
*
* $Log: pyevnt.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:26  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE PYEVNT
 
C...Administers the generation of a high-pT event via calls to
C...a number of subroutines.
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/
      SAVE /PYPARS/,/PYINT1/,/PYINT2/,/PYINT5/
 
C...Initial values for some counters.
      MINT(5)=MINT(5)+1
      MINT(7)=0
      MINT(8)=0
      MINT(83)=0
      MINT(84)=MSTP(126)
      MSTU(24)=0
      MSTU70=0
      MSTJ14=MSTJ(14)
 
C...Loop over number of pileup events; check space left.
      IF(MSTP(131).LE.0) THEN
        NPILE=1
      ELSE
        CALL PYPILE(2)
        NPILE=MINT(81)
      ENDIF
      DO 170 IPILE=1,NPILE
      IF(MINT(84)+100.GE.MSTU(4)) THEN
        CALL LUERRM(11,
     &  '(PYEVNT:) no more space in LUJETS for pileup events')
        IF(MSTU(21).GE.1) GOTO 180
      ENDIF
      MINT(82)=IPILE
 
C...Generate variables of hard scattering.
  100 CONTINUE
      MINT(31)=0
      MINT(51)=0
      CALL PYRAND
      ISUB=MINT(1)
      IF(MSTP(111).EQ.-1) GOTO 160
 
      IF(ISUB.LE.90.OR.ISUB.GE.95) THEN
C...Hard scattering (including low-pT):
C...reconstruct kinematics and colour flow of hard scattering.
        CALL PYSCAT
        IF(MINT(51).EQ.1) GOTO 100
        IPU1=MINT(84)+1
        IPU2=MINT(84)+2
        IF(ISUB.EQ.95) GOTO 110
 
C...Showering of initial state partons (optional).
        IF(MSTP(61).GE.1.AND.MINT(44).GE.2) CALL PYSSPA(IPU1,IPU2)
 
C...Showering of final state partons (optional).
        IF(MSTP(71).GE.1.AND.ISET(ISUB).GE.2) THEN
          IPU3=MINT(84)+3
          IPU4=MINT(84)+4
          IF(ISET(ISUB).EQ.6) IPU4=-3
          QMAX=SQRT(PARP(71)*VINT(52))
          IF(ISET(ISUB).GE.3.AND.ISET(ISUB).LE.5) QMAX=PMAS(23,1)
          IF(ISUB.EQ.8.OR.ISUB.EQ.76.OR.ISUB.EQ.77.OR.ISUB.EQ.124.OR.
     &    ISUB.EQ.174.OR.ISUB.EQ.179) QMAX=PMAS(24,1)
          CALL LUSHOW(IPU3,IPU4,QMAX)
        ENDIF
 
C...Decay of final state resonances.
        IF(MSTP(41).GE.1) CALL PYRESD
        IF(MINT(51).EQ.1) GOTO 100
        MINT(52)=N
 
C...Multiple interactions.
        IF(MSTP(81).GE.1.AND.MINT(44).EQ.4) CALL PYMULT(6)
        MINT(53)=N
 
C...Hadron remnants and primordial kT.
  110   CALL PYREMN(IPU1,IPU2)
        IF(MINT(51).EQ.1) GOTO 100
 
      ELSE
C...Diffractive and elastic scattering.
        CALL PYDIFF
      ENDIF
 
C...Recalculate energies from momenta and masses (if desired).
      IF(MSTP(113).GE.1) THEN
        DO 120 I=MINT(83)+1,N
  120   IF(K(I,1).GT.0.AND.K(I,1).LE.10) P(I,4)=SQRT(P(I,1)**2+
     &  P(I,2)**2+P(I,3)**2+P(I,5)**2)
      ENDIF
 
C...Rearrange partons along strings, check invariant mass cuts.
      MSTU(28)=0
      IF(MSTP(111).LE.0) MSTJ(14)=-1
      CALL LUPREP(MINT(84)+1)
      MSTJ(14)=MSTJ14
      IF(MSTP(112).EQ.1.AND.MSTU(28).EQ.3) GOTO 100
      IF(MSTP(125).EQ.0.OR.MSTP(125).EQ.1) THEN
        DO 130 I=MINT(84)+1,N
        IF(K(I,2).NE.94) GOTO 130
        K(I+1,3)=MOD(K(I+1,4)/MSTU(5),MSTU(5))
        K(I+2,3)=MOD(K(I+2,4)/MSTU(5),MSTU(5))
  130   CONTINUE
        CALL LUEDIT(12)
        CALL LUEDIT(14)
        IF(MSTP(125).EQ.0) CALL LUEDIT(15)
        IF(MSTP(125).EQ.0) MINT(4)=0
        DO 150 I=MINT(83)+1,N
        IF(K(I,1).EQ.11.AND.K(I,4).EQ.0.AND.K(I,5).EQ.0) THEN
          DO 140 I1=I+1,N
          IF(K(I1,3).EQ.I.AND.K(I,4).EQ.0) K(I,4)=I1
  140     IF(K(I1,3).EQ.I) K(I,5)=I1
        ENDIF
  150   CONTINUE
      ENDIF
 
C...Introduce separators between sections in LULIST event listing.
      IF(IPILE.EQ.1.AND.MSTP(125).LE.0) THEN
        MSTU70=1
        MSTU(71)=N
      ELSEIF(IPILE.EQ.1) THEN
        MSTU70=3
        MSTU(71)=2
        MSTU(72)=MINT(4)
        MSTU(73)=N
      ENDIF
 
C...Perform hadronization (if desired).
      IF(MSTP(111).GE.1) THEN
        CALL LUEXEC
        IF(MSTU(24).NE.0) GOTO 100
      ENDIF
      IF(MSTP(125).EQ.0.OR.MSTP(125).EQ.1) CALL LUEDIT(14)
 
C...Store event information and calculate Monte Carlo estimates of
C...subprocess cross-sections.
  160 IF(IPILE.EQ.1) CALL PYDOCU
 
C...Set counters for current pileup event and loop to next one.
      MSTI(41)=IPILE
      IF(IPILE.GE.2.AND.IPILE.LE.10) MSTI(40+IPILE)=ISUB
      IF(MSTU70.LT.10) THEN
        MSTU70=MSTU70+1
        MSTU(70+MSTU70)=N
      ENDIF
      MINT(83)=N
      MINT(84)=N+MSTP(126)
  170 CONTINUE
 
C...Generic information on pileup events.
      IF(MSTP(131).EQ.1.AND.MSTP(133).GE.1) THEN
        PARI(91)=VINT(132)
        PARI(92)=VINT(133)
        PARI(93)=VINT(134)
        IF(MSTP(133).GE.2) PARI(93)=PARI(93)*XSEC(0,3)/VINT(131)
      ENDIF
 
C...Transform to the desired coordinate frame.
  180 CALL PYFRAM(MSTP(124))
      MSTU(70)=MSTU70
      PARU(21)=VINT(1)
 
      RETURN
      END
