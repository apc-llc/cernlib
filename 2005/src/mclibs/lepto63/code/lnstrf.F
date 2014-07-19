
C ********************************************************************

      SUBROUTINE LNSTRF(X,Q2,XPQ)

C...Structure function per nucleon for a proton/neutron mixture
C...according to defined nucleus.

      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      DIMENSION XPQ(-6:6)

      CALL LYSTFU(2212,X,Q2,XPQ)

      IF(PARI(11).LE.1.E-06) RETURN
      XDV=XPQ(1)-XPQ(-1)
      XUV=XPQ(2)-XPQ(-2)
C...For nuclear target, mix u- and d-valence distributions.
      XPQ(1)=(1.-PARI(11))*XDV+PARI(11)*XUV + XPQ(-1)
      XPQ(2)=(1.-PARI(11))*XUV+PARI(11)*XDV + XPQ(-2)
C...Save d and u valence in proton
      PARI(12)=XDV
      PARI(13)=XUV

      RETURN
      END
