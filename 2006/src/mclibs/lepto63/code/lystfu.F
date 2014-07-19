
C **********************************************************************

      SUBROUTINE LYSTFU(KF,X,Q2,XPQ)

C...Interface to PYSTFU in PYTHIA 5.7 to get parton density distributions,
C...i.e. momentum weighted probability distributions xq(x,Q2), xg(x,Q2).
C...Also gives intrinsic charm and beauty distributions.
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),XLP,YLP,W2LP,Q2LP,ULP
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON /ARSTRF/ KFSAVE(2),XSAVE(2),XQ2SAV(2),XPQSAV(2,-6:6)
      DIMENSION XPQ(-6:6),XPYST(-25:25)

C...Reset arrays etc.
      DO 100 KFL=-6,6
      XPQ(KFL)=0.0
  100 XPQSAV(1,KFL)=0.
      XSAVE(1)=X
      XQ2SAV(1)=Q2
      KFSAVE(1)=KF
C...Check x and particle species.
      IF(X.LE.0..OR.X.GE.1.) THEN
        WRITE(MSTU(11),5000) X
        RETURN
      ENDIF

      IF(LST(15).EQ.-4.OR.LST(15).EQ.-5) THEN
C...Intrinsic charm/bottom quark distribution in the proton...
        IF(Q2.LT.1.) RETURN
C...from Phys. Lett 93B (1980) 451, 
C...Amount of intrinsic charm PARL(12)=BETA^2
        XPQ(4)=X**3*1800.*PARL(12)*
     &         ((1.-X)/3.*(1.+10.*X+X**2)+2.*X*(1.+X)*LOG(X))
C...plus first order QCD-correction parametrized with polynomia
        IF(X.LT.0.9) THEN
          XCORR=0.22024E-1*X-0.77833E-1*X**2-0.47292*X**3+
     &          2.104*X**4-2.1698*X**5-0.84891*X**6+1.8882*X**7+
     &          0.8989*X**8-2.1072*X**9+0.76351*X**10
        ELSE
          XCORR=-1.
        ENDIF
C...and a Q2 dependence on that
CJR        XCORR=1.125*XCORR*0.190424*EXP(1.15*LOG(LOG(Q2)))
        IF(Q2.GT.1) THEN
           XCORR=1.125*XCORR*0.190424*EXP(1.15*LOG(LOG(Q2)))
        ELSE
           XCORR=1.125*XCORR*0.190424
        ENDIF
C...smooth cut-off of the structure function !
        XPQ(4)=MAX(XPQ(4)+XCORR,XPQ(4)/Q2)
        XPQ(-4)=XPQ(4)
        IF(LST(15).EQ.-5) THEN
C...Intrinsic bottom assumed to have the same shape as zeroth
C...approximation but suppressed by (mc/mb)**2=0.1 approximately
          XPQ(5)=XPQ(4)*0.1
          XPQ(-5)=XPQ(5)
          XPQ(4)=0.
          XPQ(-4)=0.
        ENDIF
      ELSE
C...Parton densities from PYSTFU in PYTHIA 5.7
        CALL PYSTFU(KF,X,Q2,XPYST)
        DO 110 KFL=-6,6
  110   XPQ(KFL)=XPYST(KFL)
      ENDIF

      DO 120 KFL=-6,6
  120 XPQSAV(1,KFL)=XPQ(KFL)

C...Formats for error printouts.
 5000 FORMAT(' Error in LYSTFU: x =',1P,E12.4,' outside physical range')

      RETURN
      END
