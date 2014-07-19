
C **********************************************************************

      SUBROUTINE FLIPOL(FLQ,FLG,FLM)

C...QCD and target mass contributions to longitudinal structure function
C...from interpolation on x,Q2 grid.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /FLGRID/ NFX,NFQ,XR(2),QR(2),FLQT(41,16),FLGT(41,16),
     &FLMT(41,16)
      DATA NOUT/0/,NWARN/10/

      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      LHT=LST(11)/100
      XP=X
      Q2P=Q2
C...NOTE: tiny mismatch between present x-value and those on grid.
      QR(2)=X*PARL(21)
      IF(QR(1).GT.QR(2)) RETURN
      IF(X.LT.XR(1).OR.X.GT.XR(2).OR.
     &Q2.LT.QR(1).OR.Q2.GT.QR(2)) THEN
C...x and/or Q2 outside grid limits, write warning for first NWARN cases
        IF(LST(2).GE.0) THEN
          NOUT=NOUT+1
          IF(LST(3).GE.1.AND.NOUT.LE.NWARN) WRITE(6,1000) X,Q2,NWARN
        ENDIF
        IF(X.LT.XR(1)) XP=XR(1)
        IF(X.GT.XR(2)) XP=XR(2)
        IF(Q2.LT.QR(1)) Q2P=QR(1)
        IF(Q2.GT.QR(2)) Q2P=QR(2)
      ENDIF

      IX=(ALOG10(XP)-ALOG10(XR(1)))/
     &(ALOG10(XR(2))-ALOG10(XR(1)))*(NFX-1)+1
      IQ=(ALOG10(Q2P)-ALOG10(QR(1)))/
     &(ALOG10(QR(2))-ALOG10(QR(1)))*(NFQ-1)+1
      IX=MIN(IX,NFX-1)
      IQ=MIN(IQ,NFQ-1)
      Q2L=10**(ALOG10(QR(1))+(ALOG10(QR(2))-ALOG10(QR(1)))*
     &(IQ-1)/(NFQ-1))
      Q2H=10**(ALOG10(QR(1))+(ALOG10(QR(2))-ALOG10(QR(1)))*
     &(IQ  )/(NFQ-1))
      XL=10**(ALOG10(XR(1))+(ALOG10(XR(2))-ALOG10(XR(1)))*
     &(IX-1)/(NFX-1))
      XH=10**(ALOG10(XR(1))+(ALOG10(XR(2))-ALOG10(XR(1)))*
     &(IX  )/(NFX-1))
      QD=(Q2P-Q2L)/(Q2H-Q2L)
      XD=(XP-XL)/(XH-XL)

      IF(LQCD.EQ.1) THEN
        X1P=(FLQT(IX+1,IQ)-FLQT(IX,IQ))*XD+FLQT(IX,IQ)
        X2P=(FLQT(IX+1,IQ+1)-FLQT(IX,IQ+1))*XD+FLQT(IX,IQ+1)
        FLQ=(X2P-X1P)*QD+X1P
        X1P=(FLGT(IX+1,IQ)-FLGT(IX,IQ))*XD+FLGT(IX,IQ)
        X2P=(FLGT(IX+1,IQ+1)-FLGT(IX,IQ+1))*XD+FLGT(IX,IQ+1)
        FLG=(X2P-X1P)*QD+X1P
      ENDIF
      IF(LTM.EQ.1) THEN
        X1P=(FLMT(IX+1,IQ)-FLMT(IX,IQ))*XD+FLMT(IX,IQ)
        X2P=(FLMT(IX+1,IQ+1)-FLMT(IX,IQ+1))*XD+FLMT(IX,IQ+1)
        FLM=(X2P-X1P)*QD+X1P
      ENDIF

      RETURN
 1000 FORMAT(' Warning: x=',F7.4,' or Q2=',F6.1,' outside grid,',
     &' for FL interpolation',/,10X,'value on grid limit used.',
     &' Only first',I5,' warnings printed.',/)
      END
