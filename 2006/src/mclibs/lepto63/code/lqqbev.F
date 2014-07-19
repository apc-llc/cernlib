
C **********************************************************************

      SUBROUTINE LQQBEV

C...Generate boson-gluon fusion event, choose xp and zp according to
C...QCD matrix elements and apply cuts for softness and collinearness.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)

      LST(24)=3
      W=SQRT(W2)
      J1=MSTU(1)
      J2=MSTU(1)+1
      J3=MSTU(1)+2
      J4=MSTU(1)+3

      CALL LXP(XP,IFAIL)
      IF(IFAIL.NE.0) GOTO 999

C...Choose flavour of produced quark-antiquark pair.
  200 CALL LFLAV(IFL1,IFL3)
      IF(LST(21).NE.0) RETURN
      IF(IFL1.LT.0) THEN
C...Put quark in first position
        IFL1S=IFL1
        IFL1=IFL3
        IFL3=IFL1S
      ENDIF
      CALL LZP(XP,ZP,IFAIL)
      IF(IFAIL.NE.0) GOTO 999
      IFL1A=IABS(IFL1)
      IFL3A=IABS(IFL3)
      MSTJ(93)=1
      AMIFL1=ULMASS(IFL1)
      MSTJ(93)=1
      AMIFL3=ULMASS(IFL3)

      IF(LST(14).EQ.0.OR.(LST(8).GE.2.AND.MOD(LST(8),10).NE.9)) THEN
C...If baryon production from target remnant is neglected the
C...target remnant is approximated by a gluon.
        IF(W.LT.AMIFL1+AMIFL3+PARJ(32)) GOTO 999
        IF(LQMCUT(XP,ZP,AMIFL1,0.,AMIFL3).NE.0) GOTO 999
        CALL LU3ENT(J1,IFL1,21,IFL3,W,PARI(21),PARI(23))
        K(MSTU(1)+1,3)=2
C...Align target remnant (gluon) along -z axis
        CALL LUROBO(-ACOS(-P(J2,3)/SQRT(P(J2,3)**2+P(J2,1)**2)),
     &  0.,0.,0.,0.)
C...Phi-rotation to bring quark to phi=0.
        CALL LUROBO(0.,-PLU(J1,15),0.,0.,0.)
      ELSE

        IF(W.LT.AMIFL1+AMIFL3+0.9+2.*PARJ(32)) GOTO 999
        IF(LQMCUT(XP,ZP,AMIFL1,1.,AMIFL3).NE.0) GOTO 999
        P(J1,5)=AMIFL1
        P(J3,5)=AMIFL3
C...Choose target valence quark/diquark to form jet system with
C...produced antiquark/quark.
        IFLR2=INT(1.+LST(22)/3.+RLU(0))
        IF(IFLR2.EQ.LST(22)) THEN
          IFLR1=2101
          IF(RLU(0).GT.PARL(4)) IFLR1=2103
        ELSE
          IFLR1=1000*IFLR2+100*IFLR2+3
        ENDIF
        IFLR2=3-IFLR2
        MSTJ(93)=1
        AMR1=ULMASS(IFLR1)
CJR--
        KFIFL1=LUCOMP(IFLR1)
        IF (KFIFL1.EQ.90) THEN
          AMR1=AMR1-2*PARL(20)
        ELSEIF (1.LE.KFIFL1 .AND. KFIFL1.LE.6) THEN
          AMR1=AMR1-PARL(20)
        ENDIF
        MSTJ(93)=1
        AMR2=ULMASS(IFLR2)
        KFIFL2=LUCOMP(IFLR2)
        IF (KFIFL2.EQ.90) THEN
          AMR2=AMR2-2.*PARL(20)
        ELSEIF (1.LE.KFIFL2 .AND. KFIFL2.LE.6) THEN
          AMR2=AMR2-PARL(20)
        ENDIF
CJR--
        NREMH=0
  310   NREMH=NREMH+1
        IF(NREMH.GT.100) GOTO 999
        CALL LPRIKT(PARL(14),PT,PHI)
        CALL LREMH(0,PT,IFLR1,IFLR2,XT)
        PT2=PT**2
        TM2R1=AMR1**2+PT2
        TM2R2=AMR2**2+PT2
        P(J2,5)=SQRT(TM2R1/(1.-XT)+TM2R2/XT)
        IF(LQMCUT(XP,ZP,AMIFL1,P(J2,5),AMIFL3).NE.0) GOTO 310
        MSTU(10)=1
        CALL LU3ENT(J1,IFL1,21,IFL3,W,PARI(21),PARI(23))
        MSTU(10)=2
C...Align target remnant (effective gluon) along -z axis
        CALL LUROBO(-ACOS(-P(J2,3)/SQRT(P(J2,3)**2+P(J2,1)**2)),
     &0.,0.,0.,0.)
C...Phi-rotation to bring quark to phi=0.
        CALL LUROBO(0.,-PLU(J1,15),0.,0.,0.)
        EPZ=P(J2,4)-P(J2,3)
        IF(IFL1.GT.0) THEN
          IR1=J2
          IR2=J4
        ELSE
          IR1=J4
          IR2=J2
        ENDIF
        P(IR1,1)=PT*COS(PHI)
        P(IR1,2)=PT*SIN(PHI)
        P(IR1,3)=-0.5*((1.-XT)*EPZ-TM2R1/(1.-XT)/EPZ)
        P(IR1,4)= 0.5*((1.-XT)*EPZ+TM2R1/(1.-XT)/EPZ)
        P(IR1,5)=AMR1
        P(IR2,1)=-P(IR1,1)
        P(IR2,2)=-P(IR1,2)
        P(IR2,3)=-0.5*(XT*EPZ-TM2R2/XT/EPZ)
        P(IR2,4)= 0.5*(XT*EPZ+TM2R2/XT/EPZ)
        P(IR2,5)=AMR2
        K(IR1,1)=1
        K(IR1,2)=IFLR1
        K(IR2,1)=1
        K(IR2,2)=IFLR2
        K(J3,1)=2
        DO 320 I=J1,J4
          DO 320 J=3,5
  320       K(I,J)=0
        N=J4
        K(IR1,3)=2
        K(IR2,3)=2
       IF((P(J1,4)+P(J2,4))**2-(P(J1,1)+P(J2,1))**2-(P(J1,3)+P(J2,3))**2
     &  -P(J2,2)**2.LT.(P(J1,5)+P(J2,5)+PARJ(32))**2) GOTO 310
       IF((P(J3,4)+P(J4,4))**2-(P(J3,1)+P(J4,1))**2-(P(J3,3)+P(J4,3))**2
     &  -P(J4,2)**2.LT.(P(J3,5)+P(J4,5)+PARJ(32))**2) GOTO 310

      ENDIF

      CALL LAZIMU(XP,ZP)
      LST(21)=0
      RETURN

  999 LST(21)=5
      RETURN
      END
