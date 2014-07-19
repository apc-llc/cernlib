CDECK  ID>, HWDHQK. 
*CMZ :-        -09/12/92  10.51.11  by  Bryan Webber
*-- Author :    Ian Knowles & Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWDHQK
C-----------------------------------------------------------------------
C     Performs weak decays of heavy quarks: 0 -> W + 3,  W -> 1 + 2(bar)
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWULDO,RN,BF,EMMX,EMWSQ,GMWSQ,EMLIM,PW(4),
     & EMTST
      INTEGER IST(3),IHEP,IS,ID,IM,I,LHEP,MHEP
      LOGICAL FOUND
      EXTERNAL HWRGEN,HWDWWT,HWULDO,HWDPWT
      DATA IST/113,114,114/
      IF (IERROR.NE.0) RETURN
  10  FOUND=.FALSE.
      DO 60 IHEP=1,NMXHEP
      IS=ISTHEP(IHEP)
      ID=IDHW(IHEP)
      IF (IS.GE.147.AND.IS.LE.151.AND.(ID.EQ.6.OR.ID.EQ.12.OR.
     &    (ID.GE.209.AND.ID.LE.212).OR.(ID.GE.215.AND.ID.LE.218))) THEN
        FOUND=.TRUE.
C Select decay mode
        RN=HWRGEN(0)
        BF=0.
        IM=LSTRT(ID)
        DO 20 I=1,NMODES(ID)
        BF=BF+BRFRAC(IM)
        IF (BF.GE.RN) GOTO 30
  20    IM=LNEXT(IM)
        CALL HWWARN('HWDHQK',50,*30)
  30    IF (NHEP+4.GT.NMXHEP) CALL HWWARN('HWDHQK',100,*999)
C Make a copy of decaying heavy (anti-)quark
        NHEP=NHEP+1
        ISTHEP(NHEP)=155
        IDHW(NHEP)=IDHW(IHEP)
        IDHEP(NHEP)=IDHEP(IHEP)
        CALL HWVEQU(5,PHEP(1,IHEP),PHEP(1,NHEP))
        CALL HWVEQU(4,VHEP(1,IHEP),VHEP(1,NHEP))
        IF (IDHEP(IHEP).GT.0) THEN
C Re-set pointer of quark's colour mother
          IF (JDAHEP(2,JMOHEP(2,IHEP)).NE.IHEP)
     &      CALL HWWARN('HWDHQK',98,*999)
          JDAHEP(2,JMOHEP(2,IHEP))=NHEP
C and pointer of flavour partner
          IF (JMOHEP(2,JDAHEP(2,IHEP)).EQ.IHEP)
     &      JMOHEP(2,JDAHEP(2,IHEP))=NHEP
        ELSE
C Re-set pointer of anti-quark's colour daughter
          IF (JMOHEP(2,JDAHEP(2,IHEP)).NE.IHEP)
     &      CALL HWWARN('HWDHQK',99,*999)
          JMOHEP(2,JDAHEP(2,IHEP))=NHEP
C and pointer of anti-flavour partner
          IF (JDAHEP(2,JMOHEP(2,IHEP)).EQ.IHEP)
     &      JDAHEP(2,JMOHEP(2,IHEP))=NHEP
        ENDIF
        JMOHEP(1,NHEP)=JMOHEP(1,IHEP)
        JMOHEP(2,NHEP)=JMOHEP(2,IHEP)
        JDAHEP(1,NHEP)=NHEP+1
        JDAHEP(2,NHEP)=NHEP+3
C Relabel original track
        ISTHEP(IHEP)=3
        JMOHEP(2,IHEP)=JMOHEP(1,IHEP)
        JDAHEP(1,IHEP)=NHEP
        JDAHEP(2,IHEP)=NHEP
C Label decaying quark and weak decay product jets
        LHEP=NHEP
        MHEP=NHEP+3
        DO 40 I=1,3
        NHEP=NHEP+1
        MHEP=MHEP-1
        IDHW(NHEP)=IDKPRD(I,IM)
        IDHEP(NHEP)=IDPDG(IDKPRD(I,IM))
        ISTHEP(NHEP)=IST(I)
        JMOHEP(1,NHEP)=LHEP
        JMOHEP(2,NHEP)=MHEP
        JDAHEP(1,NHEP)=0
        JDAHEP(2,NHEP)=MHEP
  40    PHEP(5,NHEP)=RMASS(IDKPRD(I,IM))
C Assign production vertex to 3 (and W)
        CALL HWUDKL(ID,PHEP(1,IHEP),VHEP(1,NHEP))
        CALL HWVSUM(4,VHEP(1,IHEP),VHEP(1,NHEP),VHEP(1,NHEP))
        IF (NME(IM).EQ.130) THEN
C Generate decay momenta using full (V-A)*(V-A) matrix element
          EMMX=PHEP(5,IHEP)-PHEP(5,NHEP)
          EMWSQ=RMASS(198)**2
          GMWSQ=(RMASS(198)*GAMW)**2
          EMLIM=GMWSQ
          IF (EMMX.LT.RMASS(198)) EMLIM=EMLIM+(EMWSQ-EMMX**2)**2
  50      CALL HWDTHR(PHEP(1,IHEP),PHEP(1,NHEP-1),
     &                PHEP(1,NHEP-2),PHEP(1,NHEP),HWDWWT)
          CALL HWVSUM(4,PHEP(1,NHEP-2),PHEP(1,NHEP-1),PW)
          EMTST=(EMWSQ-HWULDO(PW,PW))**2
          IF ((EMTST+GMWSQ)*HWRGEN(1).GT.EMLIM) GOTO 50
        ELSE
C Use phase space
          CALL HWDTHR(PHEP(1,IHEP),PHEP(1,NHEP-2),
     &                PHEP(1,NHEP-1),PHEP(1,NHEP),HWDPWT)
          CALL HWVSUM(4,PHEP(1,NHEP-2),PHEP(1,NHEP-1),PW)
        ENDIF
C Assign production verticies to 1 and 2
        CALL HWUDKL(198,PW,VHEP(1,NHEP-2))
        CALL HWVSUM(4,VHEP(1,NHEP),VHEP(1,NHEP-2),VHEP(1,NHEP-2))
        CALL HWVEQU(4,VHEP(1,NHEP-2),VHEP(1,NHEP-1))
C Do parton showers
        EMSCA=PHEP(5,IHEP)
        CALL HWBGEN
      ENDIF
      IF (IHEP.EQ.NHEP) GOTO 70
  60  CONTINUE
  70  IF (FOUND) THEN
C Go back to check for further heavy decay products or other heavy quarks
        GOTO 10
      ENDIF
  999 END
