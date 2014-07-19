
C **********************************************************************

      FUNCTION LQMCUT(XP,ZP,AM1,AM2,AM3)

C...Apply cuts, if necessary, on the event configuration
C...obtained from QCD matrix elements.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      DATA S12,S23,S13/3*0./

      IF(LST(24).EQ.2) THEN
        S12=Q2*(1.-XP)/XP
        S23=Q2*(XP-X)*(1.-ZP)/X/XP+AM2**2+AM3**2
        S13=Q2*(XP-X)*ZP/X/XP+AM1**2+AM3**2
      ELSEIF(LST(24).EQ.3) THEN
        S13=Q2*(1.-XP)/XP
        S23=Q2*(XP-X)*(1.-ZP)/X/XP+AM2**2+AM3**2
        S12=Q2*(XP-X)*ZP/X/XP+AM1**2+AM2**2
        IF(S13.LT.(AM1+AM3)**2) GOTO 900
      ENDIF

      W=SQRT(W2)
      X1=1.-(S23-AM1**2)/W2
      X3=1.-(S12-AM3**2)/W2
      X2=2.-X1-X3
      PARI(21)=X1
      PARI(22)=X2
      PARI(23)=X3
      
           
      IF(X1.GT.1..OR.X2.GT.1..OR.X3.GT.1.) GOTO 900
      IF(X1*W/2..LT.AM1.OR.X2*W/2..LT.AM2.OR.X3*W/2..LT.AM3) GOTO 900
      PA1=SQRT((0.5*X1*W)**2-AM1**2)
      PA2=SQRT((0.5*X2*W)**2-AM2**2)
      PA3=SQRT((0.5*X3*W)**2-AM3**2)
      IF(ABS((PA3**2-PA1**2-PA2**2)/(2.*PA1*PA2)).GE.1.) GOTO 900
      IF(ABS((PA2**2-PA1**2-PA3**2)/(2.*PA1*PA3)).GE.1.) GOTO 900
      LQMCUT=0
      RETURN

  900 LQMCUT=1
      RETURN
      END
