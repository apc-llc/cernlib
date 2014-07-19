CDECK  ID>, HWHIG1.
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    Ulrich Baur & Nigel Glover, adapted by Ian Knowles
C-----------------------------------------------------------------------
      FUNCTION HWHIG1(S,T,U,EH2,EQ2,I,J,K,I1,J1,K1)
C-----------------------------------------------------------------------
C     Basic matrix elements for Higgs + jet production; used in HWHIGA
C-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE COMPLEX HWHIG1,HWHIG2,HWHIG5,BI(4),CI(7),DI(3)
      DOUBLE PRECISION S,T,U,EH2,EQ2,S1,T1,U1,ONE,TWO,FOUR,HALF
      INTEGER I,J,K,I1,J1,K1
      COMMON/CINTS/BI,CI,DI
      PARAMETER (ONE =1.D0, TWO =2.D0, FOUR =4.D0, HALF =0.5D0)
C-----------------------------------------------------------------------
C     +++ helicity amplitude for: g+g --> g+H
C-----------------------------------------------------------------------
      S1=S-EH2
      T1=T-EH2
      U1=U-EH2
      HWHIG1=EQ2*FOUR*DSQRT(TWO*S*T*U)*(
     & -FOUR*(ONE/(U*T)+ONE/(U*U1)+ONE/(T*T1))
     & -FOUR*((TWO*S+T)*BI(K)/U1**2+(TWO*S+U)*BI(J)/T1**2)/S
     & -(S-FOUR*EQ2)*(S1*CI(I1)+(U-S)*CI(J1)+(T-S)*CI(K1))/(S*T*U)
     & -8.D0*EQ2*(CI(J1)/(T*T1)+CI(K1)/(U*U1))
     & +HALF*(S-FOUR*EQ2)*(S*T*DI(K)+U*S*DI(J)-U*T*DI(I))/(S*T*U)
     & +FOUR*EQ2*DI(I)/S
     & -TWO*(U*CI(K)+T*CI(J)+U1*CI(K1)+T1*CI(J1)-U*T*DI(I))/S**2 )
      RETURN
C-----------------------------------------------------------------------
      ENTRY HWHIG2(S,T,U,EH2,EQ2,I,J,K,I1,J1,K1)
C-----------------------------------------------------------------------
C     ++- helicity amplitude for: g+g --> g+H
C-----------------------------------------------------------------------
      S1=S-EH2
      T1=T-EH2
      U1=U-EH2
      HWHIG2=EQ2*FOUR*DSQRT(TWO*S*T*U)*(FOUR*EH2
     & +(EH2-FOUR*EQ2)*(S1*CI(4)+T1*CI(5)+U1*CI(6))
     & -HALF*(EH2-FOUR*EQ2)*(S*T*DI(3)+U*S*DI(2)+U*T*DI(1)) )/(S*T*U)
      RETURN
C-----------------------------------------------------------------------
      ENTRY HWHIG5(S,T,U,EH2,EQ2,I,J,K,I1,J1,K1)
C-----------------------------------------------------------------------
C     Amplitude for: q+qbar --> g+H
C-----------------------------------------------------------------------
      HWHIG5=DCMPLX(TWO+TWO*S/(S-EH2))*BI(I)+DCMPLX(FOUR*EQ2-U-T)*CI(K)
      RETURN
      END
