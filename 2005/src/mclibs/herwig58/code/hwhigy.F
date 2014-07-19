*
* $Id: hwhigy.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhigy.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHIGY.
*CMZ :-        -26/04/91  13.37.37  by  Federico Carminati
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWHIGY(A,B,XP)
C-----------------------------------------------------------------------
C     CALCULATE THE INTEGRAL OF BERENDS AND KLEISS APPENDIX B
C-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION ZERO, ONE, TWO, THREE, HALF
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0,
     &           THREE=3.D0, HALF=0.5D0)
      DOUBLE PRECISION HWHIGY,A,B,XP,Y
      COMPLEX XQ,Z1,Z2,Z3,Z4,C0,C1,C2,C3,C4,C5,C6,C7,C8,FUN,Z
C---DECLARE ALL THE STATEMENT-FUNCTION DEFINITIONS
      C0(Z,A)=(Z**2-A)**2*((Z**2+A)**2-24*Z*(Z**2+A)+8*Z**2*(A+6))/Z**4
      C1(Z,A)=A**4/(3*Z)
      C2(Z,A)=-A**3*(24*Z-A)/(2*Z**2)
      C3(Z,A)=A**2*(8*Z**2*(A+6)-24*A*Z+A**2)/Z**3
      C4(Z,A)=-A**2*(24*Z**3+8*Z**2*(A+6)-24*A*Z+A**2)/Z**4
      C5(Z,A)=Z**3-24*Z**2+8*Z*(A+6)+24*A
      C6(Z,A)=0.5*Z**2-12*Z+4*(A+6)
      C7(Z,A)=Z/3-8
      C8(Z,A)=0.25
      FUN(Z,Y,A)=C0(Z,A)*LOG(Y-Z)
     &          +C1(Z,A)/Y**3
     &          +C2(Z,A)/Y**2
     &          +C3(Z,A)/Y
     &          +C4(Z,A)*LOG(Y)
     &          +C5(Z,A)*Y
     &          +C6(Z,A)*Y**2
     &          +C7(Z,A)*Y**3
     &          +C8(Z,A)*Y**4
C---NOW EVALUATE THE INTEGRAL
      HWHIGY=0
      IF (A.GT.4) RETURN
      XQ=CMPLX(XP,B)
      Z1=XQ+SQRT(XQ**2-A)
      Z2=XQ-SQRT(XQ**2-A)
      Z3=FUN(Z1,TWO,A)-FUN(Z1,SQRT(A),A)
      Z4=FUN(Z2,TWO,A)-FUN(Z2,SQRT(A),A)
      HWHIGY=AIMAG((Z3-Z4)/(Z1-Z2))/(8*B)
      END
