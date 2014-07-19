
C **********************************************************************

      SUBROUTINE LXP(XP,IFAIL)

C...Choose value of XP according to QCD matrix elements weighted by
C...structure functions.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      DOUBLE PRECISION DXPMAX

      IFAIL=1
CAE.Scheme for ME cutoff: W2, Q2, mixed, z-shat
      IF(LST(20).LE.1) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*(1.D0-DBLE(X))*DBLE(PARL(27)))
        DXPMAX=DBLE(X)/(DBLE(X)+(1.D0-DBLE(X))*DBLE(PARL(27)))
        XPMAX=SNGL(DXPMAX)
      ELSEIF(LST(20).EQ.2) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*DBLE(X)*DBLE(PARL(27)))
        DXPMAX=1.D0/(1.D0+DBLE(PARL(27)))
        XPMAX=SNGL(DXPMAX)
      ELSEIF(LST(20).EQ.3.OR.LST(20).EQ.4) THEN
        XPMIN=X
        DXPMAX=1.D0/(1.D0+DBLE(PARL(9)))
        XPMAX=DXPMAX
      ELSEIF(LST(20).EQ.5) THEN
        XPMIN=X
        DXPMAX=DBLE(Q2)/(DBLE(Q2)+DBLE(PARL(9)))
        XPMAX=DXPMAX
      ELSEIF(LST(20).EQ.6) THEN
        XPMIN=X
        DXPMAX=DBLE(Q2)/(DBLE(Q2)+DBLE(PARL(27)))
        XPMAX=DXPMAX
      ELSE
        WRITE(6,*) 'LXP: No such jet scheme!'
      ENDIF
CAE
      IF(XPMIN.LT.X.OR.XPMIN.GT.1.) RETURN
      IF(XPMIN.GE.XPMAX) RETURN
      AP=1.-XPMIN
      BP=(1.D0-DXPMAX)/AP
      IF(LST(24).EQ.2) THEN
        QXPMAX=PARI(15)
        IF(LST(17).NE.0.AND.LST(19).GE.0) QXPMAX=
     &  PARI(24)*PARI(15)+PARI(25)*PARI(16)+PARI(26)*PARI(17)
      ELSE
        QXPMAX=PARI(18)
        IF(LST(17).NE.0.AND.LST(19).GE.0) QXPMAX=
     &  PARI(24)*PARI(18)+PARI(25)*PARI(19)
      ENDIF
C...Safety factor on max value.
      QXPMAX=QXPMAX*1.05
      LOOP=0
  100 LOOP=LOOP+1
      IF(LOOP.GT.1000) RETURN
      XP=1.-AP*BP**RLU(0)
      XPWEIT=DSIGMA(XP)/QXPMAX
      IF(XPWEIT.LT.RLU(0)) GOTO 100
      IFAIL=0
      RETURN
      END
