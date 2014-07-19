C
C*********************************************************************
C
      SUBROUTINE SASDIR(X,Q2,P2,Q02,XPGA)
C...Purpose: to evaluate the direct contribution, i.e. the C^gamma term,
C...as needed in MSbar parametrizations.
      DIMENSION XPGA(-6:6)
      DATA AEM2PI/0.0011614/
C
C...Reset output.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE
C
C...Evaluate common x-dependent expression.
      XTMP = (X**2+(1.-X)**2) * (-LOG(X)) - 1.
      CGAM = 3.*AEM2PI*X * (XTMP*(1.+P2/(P2+Q02)) + 6.*X*(1.-X))
C
C...d, u, s part by simple charge factor.
      XPGA(1)=(1./9.)*CGAM
      XPGA(2)=(4./9.)*CGAM
      XPGA(3)=(1./9.)*CGAM
C
C...Also fill for antiquarks.
      DO 110 KF=1,5
      XPGA(-KF)=XPGA(KF)
  110 CONTINUE
C
      RETURN
      END
