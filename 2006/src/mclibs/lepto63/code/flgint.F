
C **********************************************************************

      FUNCTION FLGINT(Z)

C...Gluon contribution integrand to QCD longitudinal structure function.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LINTEG/ NTOT,NPASS
      DIMENSION XPQ(-6:6)
      DATA PI/3.14159/
      NTOT=NTOT+1
      CALL LNSTRF(Z,Q2,XPQ)
      FLGINT=20./9.*PARL(25)/PI*(X/Z)**2*(1.-X/Z)/Z*XPQ(0)
      NPASS=NPASS+1

      RETURN
      END
