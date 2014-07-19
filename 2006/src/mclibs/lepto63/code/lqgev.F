
C **********************************************************************

      SUBROUTINE LQGEV

C...Generate quark-gluon jet event, choose xp and zp according to QCD
C...matrix elements and apply cuts for soft and collinear gluons.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)

      LST(24)=2
      W=SQRT(W2)
      J1=MSTU(1)
      J2=MSTU(1)+1
      J3=MSTU(1)+2
      J4=MSTU(1)+3

      CALL LXP(XP,IFAIL)
      IF(IFAIL.NE.0) GOTO 999

C...Choose flavour of scattered quark and target remnant.
  200 CALL LFLAV(IFL,IFLR)
      IF(LST(21).NE.0) RETURN
      CALL LZP(XP,ZP,IFAIL)
      IF(IFAIL.NE.0) GOTO 999
      MSTJ(93)=1
      AMIFL=ULMASS(IFL)
      MSTJ(93)=1
      AMIFLR=ULMASS(IFLR)
      
      IF(LST(14).EQ.0.OR.IFLR.GT.10
     &.OR.(LST(8).GE.2.AND.MOD(LST(8),10).NE.9)) THEN
        IF(W.LT.AMIFL+AMIFLR+PARJ(32)) GOTO 999
        IF(LQMCUT(XP,ZP,AMIFL,0.,AMIFLR).NE.0) GOTO 999
        CALL LU3ENT(J1,IFL,21,IFLR,W,PARI(21),PARI(23))
        K(MSTU(1)+2,3)=2
        CALL LUROBO(ACOS(-P(J3,3)/SQRT(P(J3,3)**2+P(J3,1)**2)),
     &  0.,0.,0.,0.)
      ELSE
C...Target remnant is not a simple diquark, special treatment needed.
        IF(W.LT.AMIFL+AMIFLR+1.+PARJ(32)) GOTO 999
        IF(LQMCUT(XP,ZP,AMIFL,0.,1.).NE.0) GOTO 999
        IFLRO=IFLR
        NREMH=0
  300   NREMH=NREMH+1
        IF(NREMH.GT.100) GOTO 999
        CALL LPRIKT(PARL(14),PT,PHI)
        CALL LREMH(IFLRO,PT,IFLR,K2,XT)
        MSTJ(93)=1
        AMIFLR=ULMASS(IFLR)
CJR--
        KFIFLR=LUCOMP(IFLR)
        IF (KFIFLR.EQ.90) THEN
          AMIFLR=AMIFLR-2.*PARL(20)
        ELSEIF (1.LE.KFIFLR .AND. KFIFLR.LE.6) THEN
          AMIFLR=AMIFLR-PARL(20)
        ENDIF
        MSTJ(93)=1
        AMK2=ULMASS(K2)
        KFK2=LUCOMP(K2)
        IF (KFK2.EQ.90) THEN
          AMK2=AMK2-2.*PARL(20)
        ELSEIF (1.LE.KFK2 .AND. KFK2.LE.6) THEN
          AMK2=AMK2-PARL(20)
        ENDIF
CJR--
        P(J1,5)=AMIFL
        P(J2,5)=0.
        PT2=PT**2
        TM2K2=AMK2**2+PT2
        TMIFLR=AMIFLR**2+PT2
        P(J3,5)=SQRT(TM2K2/XT+TMIFLR/(1.-XT))
        IF(LQMCUT(XP,ZP,AMIFL,0.,P(J3,5)).NE.0) GOTO 300
        MSTU(10)=1
        CALL LU3ENT(J1,IFL,21,IFLR,W,PARI(21),PARI(23))
        K(MSTU(1)+2,3)=2
        MSTU(10)=2
        CALL LUROBO(ACOS(-P(J3,3)/SQRT(P(J3,3)**2+P(J3,1)**2)),
     &  0.,0.,0.,0.)
        EPZ=P(J3,4)-P(J3,3)
        P(J3,1)=PT*COS(PHI)
        P(J3,2)=PT*SIN(PHI)
        P(J3,3)=-0.5*((1.-XT)*EPZ-TMIFLR/(1.-XT)/EPZ)
        P(J3,4)= 0.5*((1.-XT)*EPZ+TMIFLR/(1.-XT)/EPZ)
        P(J3,5)=AMIFLR
        P(J4,1)=-P(J3,1)
        P(J4,2)=-P(J3,2)
        P(J4,3)=-0.5*(XT*EPZ-TM2K2/XT/EPZ)
        P(J4,4)= 0.5*(XT*EPZ+TM2K2/XT/EPZ)
        P(J4,5)=AMK2
        K(J4,1)=1
        K(J4,2)=K2
        K(J4,3)=2
        K(J4,4)=0
        K(J4,5)=0
        N=J4
        IF((P(J3,4)+P(J2,4)/2.)**2-(P(J3,1)+P(J2,1)/2.)**2-P(J3,2)**2
     &  -(P(J3,3)+P(J2,3)/2.)**2.LT.(AMIFLR+2.5*PARJ(32))**2) GOTO 300
      ENDIF

      CALL LAZIMU(XP,ZP)
      LST(21)=0
      RETURN

  999 LST(21)=4
      RETURN
      END
