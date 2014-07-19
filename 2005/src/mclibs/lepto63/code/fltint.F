
C **********************************************************************

      FUNCTION FLTINT(Z)

C...Integrand for target mass correction contribution to
C...quark longitudinal structure function

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LINTEG/ NTOT,NPASS
      DIMENSION XPQ(-6:6)
      DATA PM2/0.8804/
      NTOT=NTOT+1
      CALL LNSTRF(Z,Q2,XPQ)
      FLTINT=0.
      DO 100 I=-LST(12),LST(12)
      IF(I.EQ.0) GOTO 100
      FLTINT=FLTINT+QC(IABS(I))**2*XPQ(I)
  100 CONTINUE
      FLTINT=4.*PM2/Q2*(X/Z)**2*X*FLTINT
      NPASS=NPASS+1

      RETURN
      END
