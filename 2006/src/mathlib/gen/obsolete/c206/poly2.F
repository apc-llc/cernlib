*
* $Id: poly2.F,v 1.1.1.1 1996/04/01 15:03:12 mclareni Exp $
*
* $Log: poly2.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:12  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE POLY2(A,N,ROOT,H,MAXFUN,MODE)
      COMPLEX A(*),ROOT(*)
      COMPLEX F(30),Z,CNUM,CDEN,FZ,FF,F1,F2,V,C,B(30)
      DIMENSION H(1)
      COMMON /C206ER/ ERROR
C
      IF(N.EQ.0) RETURN
      IF(ABS(A(1)).EQ.0.) GO TO 50
      E=10.
      BE=1.E7
      GUP=0.5
      N2=N-2
C*NS  NM=N-1
      NP=N+1
      M=0
      SN=SQRT(REAL(N))
      Z=CMPLX(1.,1.)
      N1=N
C   N1 IS THE DEGREE OF THE SUCCESSIVE DEFLATED POLYNOMIAL
C
C   POLYNOMIAL OF DEGREE 1
C
      IF(N.NE.1) GO TO 5
      ROOT(1)=-A(2)/A(1)
      H(1)=0.
      RETURN
    5 DO 3 K=1,NP
      H(K)=1.
    3 B(K)=A(K)
      IF(N.NE.2) GO TO 6
C
C   POLYNOMIALS OF DEGREE 2
C
      H(N)=0.
      H(N-1)=0.
    4 C=B(2)*B(2)-4.*B(1)*B(3)
      ROOT(N)=0.5*(-B(2)+SQRT(C))/B(1)
      ROOT(N-1)=-ROOT(N)-B(2)/B(1)
      IF(N.EQ.2) RETURN
      IF(REAL(B(3)).NE.0.. OR.AIMAG(B(3)).NE.0.) GO TO 12
      H(N)=0.
      IF(REAL(B(2)).EQ.0..AND.AIMAG(B(2)).EQ.0.) H(N-1)=0.
      GO TO 12
C
C   POLYNOMIALS OF DEGREE MORE THEN 2
C
    6 DO 10 K=1,N2
      IF(REAL(B(N1+1)).EQ.0..AND.AIMAG(B(N1+1)).EQ.0.) GO TO 18
      IF(MODE.NE.0) Z=ROOT(K)
      S=BE
      IF(M.GE.MAXFUN) GO TO 51
      CALL VP(F,Z,B,2,J,N1)
      M=M+1
      IF(J.EQ.2) GO TO 13
    7 D1=S
    8 CONTINUE
      NI=0.
C
C   NI IS THE NUMBER OF STEPS IN (0,1)
C
      V=Z
      FZ=F(N1+1)
      FF=FZ
      F1=F(N1)
      F2=F(N1-1)
C
    9 NI=NI+1
      IH=0
C
C   IH IS THE INDEX OF THE STEP =1,2...NI
C
      Z=V
      F(N1+1)=FF
      F(N1)=F1
      F(N1-1)=F2
   11 IH=IH+1
      IF(IH.GT.NI) GO TO 9
      T=REAL(NI-IH)/REAL(NI)
      CNUM=F(N1+1)-T*FZ
      CDEN=F(N1)*F(N1)-F(N1-1)*CNUM*2.
      CNUM=CNUM*F(N1)
      IF(ABS(CNUM).GT.E*ABS(CDEN)) GO TO 9
      Z=Z-CNUM/CDEN
      IF(M.GE.MAXFUN) GO TO 51
      CALL VP(F,Z,B,2,J,N1)
      M=M+1
      IF(J.EQ.2) GO TO 13
      S=ABS(F(N1+1))
      IF(S.LE.0.9*D1) GO TO 7
      IF(S.LT.1.E 6*ERROR) GO TO 13
      S1=ABS(F(N1))
      S2=ABS(F(N1-1))
      CZ=ABS(Z)+1.
      IF(    S1.GT.GUP*CZ*S2) GO TO 11
      IF(S.LT.0.1*CZ*S1) GO TO 11
      EPS=S1/S2
      CALL CIRCLE(Z,EPS,F,B,N1,M,MAXFUN)
      SS=1.
      S=ABS(F(N1+1))
      S1=ABS(F(N1))
      CZ=1.+ABS(Z)
      IF(S.GT.S1*CZ) SS=0.5*S1/S
      Z=Z-SS*F(N1+1)/F(N1)
      IF(M.GE.MAXFUN) GO TO 51
      CALL VP(F,Z,B,2,J,N1)
      M=M+1
      GO TO 8
   18 Z=0.
      H(K)=0.
   13 CALL DEFLAT(B,N1,Z)
      ROOT(K)=Z
   10 CONTINUE
      GO TO 4
   12 CONTINUE
C
C   ERROR BOUND
C
      DO 14 K=1,N
      IF(H(K).EQ.0.) GO TO 14
      H(K)=0.
      CALL VP(F,ROOT(K),A,2,J,N)
      S=ABS(F(N+1))
      IF(S.EQ.0..AND.ABS(F(N)).EQ.0.)GO TO 14
      CZ=ABS(ROOT(K))
      IF(J.EQ.2) GO TO 15
      S=SN*S
      GO TO 16
   15 S=SN*ERROR
   16 S1=ABS(F(N)*F(N)-F(N-1)*F(N+1)*2.)
      S1=SQRT(S1)
      IF(S.LE.10.*S1*CZ) GO TO 17
      WRITE(6,402)K
  402 FORMAT(3X,'THE BOUND FOR THE ROOT',I3,'CANNOT BE FOUND')
      GO TO 14
   17 H(K)=S/S1
   14 CONTINUE
      RETURN
   50 WRITE(6,400)
  400 FORMAT(5X,'THE FIRST COEFFICIENT A(1) IS ZERO'///)
      RETURN
   51 WRITE(6,401)M
  401 FORMAT(3X,'NUMBER OF FUNCTION EVALUATIONS =',I6///)
      RETURN
      END
