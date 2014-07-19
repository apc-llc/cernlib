*CMZ :  1.11/03 07/06/91  14.37.53  by  O.Couet
*-- Author :
      FUNCTION HTFUN1(X)
*
      DATA C1,C2,XM1,XM2,XS1,XS2/1.,0.5,0.3,0.7,0.07,0.12/
*
      A1=-0.5*((X-XM1)/XS1)**2
      A2=-0.5*((X-XM2)/XS2)**2
      X1=C1
      X2=C2
      IF(ABS(A1).GT.0.0001)X1=C1*EXP(A1)
      IF(ABS(A2).GT.0.0001)X2=C2*EXP(A2)
      HTFUN1=X1+X2
      END
      FUNCTION HTFUN2(X,Y)
      HTFUN2=100*HTFUN1(X)*HTFUN1(Y)
      END
