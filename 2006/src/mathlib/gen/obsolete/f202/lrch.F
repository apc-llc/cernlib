*
* $Id: lrch.F,v 1.1.1.1 1996/04/01 15:03:15 mclareni Exp $
*
* $Log: lrch.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:15  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE LRCH (NN,M,A,EPS,LOWER,FLAMB,KA,KAA,R)
      DIMENSION A(KA,KA), FLAMB(KA)
      DIMENSION R(KAA,KA)
      N = NN
      Z=0.0
      Y=0.
      IS=0
      L=0
      OMEGA=.5
      PHI=.5
      XN=N
      NT=INT(1.5*SQRT(XN))
      IPA=1
      IF (LOWER) 52,2,4
2     DO 3 K=1,N
      DO 3 J=1,M
      A(K,J)=-A(K,J)
3     CONTINUE
4     G=0
      ISTOP=0
      ICF=0
      IF (N) 52,48,5
5     IF (ISTOP) 52,6,48
6     IF (N-1) 7,32,7
7     IEN=1
      ASSIGN 8 TO INDEF
      ASSIGN 59 TO NOST1
      GO TO 53
8     ICF=ICF+1
      IF (Y) 10,9,11
9     Y=-EPS*1000000.
      GO TO 7
10    Y=10.*Y
      GO TO 7
11    IF (ICF-3) 13,12,13
12    Y=0.
      GO TO 7
13    IF (K-N+2) 14,14,15
14    OMEGA=(OMEGA+1.)*(OMEGA+1.)
      Y=Y/OMEGA-EPS
      IF (Y) 12,7,7
15    IF (K-N+1) 17,16,17
16    U=A(N,1)-R(N,1)-Y
      V=A(N-1,2)-R(N-1,2)
      W=X
      GO TO 21
17    IF (K-N) 21,18,21
18    IF (EPS+X) 20,20,19
19    R(N,1)=0
      GO TO 59
20    U=X+R(N-1,2)*R(N-1,2)
      V=R(N-1,1)*R(N-1,2)
      W=R(N-1,1)*R(N-1,1)
21    FH=(U+W)/2.-SQRT((U-W)*(U-W)/4.+V*V)
      IF (W-U) 23,23,22
22    FH=U-V*V/(W-FH)
23    IF (G) 52,25,24
24    FH=.99999*Y-EPS+FH
      GO TO 26
25    FH=Y+FH
26    G=1
      IF (FH) 14,27,27
27    Y=FH
      IF (ICF-1) 7,28,7
28    OMEGA=OMEGA/(OMEGA+1.)
      GO TO 7
29    Z=Z+Y
      IF (ICF) 31,30,31
30    OMEGA=(OMEGA/(OMEGA+1.5))*(OMEGA/(OMEGA+1.5))
31    OMEGA=2.*OMEGA*FH*FH/(FHA*FHA+FHB*FHB)
      PHI=.998*PHI/(.998*PHI*(1.-OMEGA)+OMEGA)
      IF (ABS(A(N,1))-EPS) 32,39,39
32    L=L+1
      FLAMB(L)=Z+A(N,1)
      IF (LOWER) 52,33,34
33    FLAMB(L)=-FLAMB(L)
34    DO 36 J=1,M
      IF (J-1-N) 35,36,36
35    KK=N-J+1
      A(KK,J)=0.
36    CONTINUE
      N=N-1
      XN=N
      NT=INT(1.5*SQRT(XN))
      PHI=.5
      OMEGA=.5
      IP=IPA
      IF (IP-N+1) 38,37,37
37    PHI=1
38    IF (N-2) 47,39,39
39    IF (IP-N) 41,40,41
40    U=A(N,1)
      V=A(N-1,2)
      W=A(N-1,1)
      FHA=U*W-V*V
      GO TO 42
41    U=R(IP+1,1)*R(IP+1,1)
      V=R(IP,2)*R(IP+1,1)
      W=R(IP,1)*R(IP,1)+R(IP,2)*R(IP,2)
      FHA=(R(IP,1)*R(IP+1,1))*(R(IP,1)*R(IP+1,1))
42    FH=FHA/((U+W)/2.+SQRT((U-W)*(U-W)/4.+V*V))
      Y=FH*PHI
      IEN=N-NT+1
      ASSIGN 43 TO INDEF
      ASSIGN 47 TO NOST1
      GO TO 53
43    IF (K-N+1) 46,44,44
44    IF (X+Y/2.) 46,46,45
45    Y=Y+X
      GO TO 47
46    OMEGA=OMEGA+1.
      Y=.8*Y
47    IS=IS+1
      GO TO 4
48    DO 51 K=1,N
      A(K,1)=Z+A(K,1)
      IF (LOWER) 52,49,51
49    DO 50 J=1,M
      A(K,J)=-A(K,J)
50    CONTINUE
51    CONTINUE
      RETURN
52    WRITE(6,66)
      RETURN
53    KK=N+M-1
      DO 54 K=IEN,KK
      DO 54 J=1,M
      R(K,J)=0.
54    CONTINUE
      DO 58 K=IEN,N
      X=A(K,1)-R(K,1)-Y
      IF (X) 55,55,56
55    GO TO INDEF, (8,43)
56    R(K,1)=SQRT(X)
      DO 57 J=2,M
      R(K,J) = (A(K,J) - R(K,J))/ R(K,1)
57    CONTINUE
      DO  58  J=2,M
      DO 58 I=J,M
      KK=K+J-1
      II=I-J+1
58    R(KK,II)=R(KK,II)+R(K,J)*R(K,I)
      GO TO NOST1, (59,47)
59    FH=R(1,1)
      FHA=FH
      FHB=FH
      IP=1
      IPA=1
      DO 65 K=1,N
      FHB=FHA
      FHA=FH
      IPA=IP
      IF (R(K,1)-FH) 60,61,61
60    FH=R(K,1)
      IP=K
61    DO 65 J=1,M
      A(K,J)=0.
      DO 62 I=J,M
      KK=K+J-1
      II=I-J+1
62    A(K,J)=A(K,J)+R(K,I)*R(KK,II)
      IF (A(K,J)) 65,63,65
63    IF (K+J-N-1) 64,64,65
64    A(K,J)=.001*EPS
65    CONTINUE
      GO TO 29
C
66    FORMAT('  ERROR IN VALUE OF BOOLEAN VARIABLE')
      END
