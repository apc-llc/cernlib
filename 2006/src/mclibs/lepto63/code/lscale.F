
C **********************************************************************

      SUBROUTINE LSCALE(INFIN,QMAX)

C...Give maximum virtuality of partons in parton showers.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
C...Power in f(x0)=(1-x0)**power used for scale x0*W2
      DATA POWER/3./
      FOUR(I,J)=P(I,4)*P(J,4)-P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)*P(J,3)

      QMAX=0.1
      IF(LST(8).GE.2.AND.LST(8).LE.5) THEN
C...Parton showers without matrix elements matching
        IF(LST(9).EQ.1) THEN
          QMAX=Q2
        ELSEIF(LST(9).EQ.2) THEN
          QMAX=W2
        ELSEIF(LST(9).EQ.3) THEN
          QMAX=SQRT(W2*Q2)
        ELSEIF(LST(9).EQ.4) THEN
          QMAX=Q2*(1.-X)
        ELSEIF(LST(9).EQ.5) THEN
          QMAX=Q2*(1.-X)*MAX(1.,LOG(1./MAX(1.E-06,X)))
        ELSEIF(LST(9).EQ.6) THEN
          X0=1.D0-(1.D0-DBLE(X))*RLU(0)**(1./(POWER+1.))
          QMAX=X0*W2
        ELSEIF(LST(9).EQ.9) THEN
          QMAX=W2**(2./3.)
        ELSE
          WRITE(6,*) ' Warning, LSCALE: LST(9)=',LST(9),' not allowed'
        ENDIF
      ELSEIF(LST(8).GT.10.AND.LST(24).EQ.1.AND.MOD(LST(8),10).NE.9) THEN
C...Parton showers added to q-event from 1st order matrix elements
        IF(LST(20).LE.1) THEN
          QMAX=PARL(27)*W2
        ELSEIF(LST(20).EQ.2) THEN
          QMAX=PARL(27)*Q2
        ELSEIF(LST(20).EQ.3) THEN
          QMAX=PARL(9)*Q2
        ELSEIF(LST(20).EQ.4) THEN
          QMAX=PARL(9)*Q2
          IF(INFIN.LT.0) QMAX=PARL(27)*Q2/X
        ELSEIF(LST(20).EQ.5) THEN
          QMAX=PARL(9)
          IF(INFIN.LT.0) QMAX=PARL(27)*Q2/X
        ELSEIF(LST(20).EQ.6) THEN
          QMAX=PARL(27)
          IF(INFIN.LT.0) QMAX=PARL(8)*Q2/X
        ELSE
          WRITE(6,*) 'LSCALE: No such jet scheme!'
        ENDIF
      ELSEIF(LST(8).GT.10.AND.MOD(LST(8),10).NE.9) THEN
C...Parton showers added to qg-/qqbar-event from 1st order matrix elements
C...Scale given by invariant mass of final parton pair
        QMAX=P(27,5)**2
        IF(INFIN.LT.0)
     &  QMAX=MAX(ABS(-Q2-2.*FOUR(25,21)),ABS(-Q2-2.*FOUR(26,21)))
      ENDIF
      QMAX=SQRT(QMAX)

      RETURN
      END
