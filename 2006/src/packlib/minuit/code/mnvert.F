*
* $Id: mnvert.F,v 1.2 1996/03/15 18:02:54 james Exp $
*
* $Log: mnvert.F,v $
* Revision 1.2  1996/03/15 18:02:54  james
*     Modified Files:
* mnderi.F eliminate possible division by zero
* mnexcm.F suppress print on STOP when print flag=-1
*          set FVAL3 to flag if FCN already called with IFLAG=3
* mninit.F set version 96.03
* mnlims.F remove arguments, not needed
* mnmigr.F VLEN -> LENV in debug print statement
* mnparm.F move call to MNRSET to after NPAR redefined, to zero all
* mnpsdf.F eliminate possible division by zero
* mnscan.F suppress printout when print flag =-1
* mnset.F  remove arguments in call to MNLIMS
* mnsimp.F fix CSTATU so status is PROGRESS only if new minimum
* mnvert.F eliminate possible division by zero
*
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNVERT(A,L,M,N,IFAIL)
#include "minuit/d506dp.inc"
CC        inverts a symmetric matrix.   matrix is first scaled to
CC        have all ones on the diagonal (equivalent to change of units)
CC        but no pivoting is done since matrix is positive-definite.
CC
#include "minuit/d506cm.inc"
      DIMENSION A(L,M) ,PP(MNI), Q(MNI),  S(MNI)
      IFAIL=0
      IF (N .LT. 1)  GO TO 100
      IF (N .GT. MAXINT)  GO TO 100
C                   scale matrix by sqrt of diag elements
      DO 8  I=1,N
      SI = A(I,I)
      IF (SI) 100,100,8
    8 S(I) = 1.0/SQRT(SI)
      DO 20 I= 1, N
      DO 20 J= 1, N
   20 A(I,J) = A(I,J) *S(I)*S(J)
C                                        . . . start main loop . . . .
      DO 65 I=1,N
      K = I
C                   preparation for elimination step1
      IF (A(K,K) .EQ. ZERO)  GO TO 100
      Q(K)=1./A(K,K)
      PP(K) = 1.0
      A(K,K)=0.0
      KP1=K+1
      KM1=K-1
      IF(KM1)100,50,40
   40 DO 49 J=1,KM1
      PP(J)=A(J,K)
      Q(J)=A(J,K)*Q(K)
   49 A(J,K)=0.
   50 IF(K-N)51,60,100
   51 DO 59 J=KP1,N
      PP(J)=A(K,J)
      Q(J)=-A(K,J)*Q(K)
   59 A(K,J)=0.0
C                   elimination proper
   60 DO 65 J=1,N
      DO 65 K=J,N
   65 A(J,K)=A(J,K)+PP(J)*Q(K)
C                   elements of left diagonal and unscaling
      DO 70 J= 1, N
      DO 70 K= 1, J
      A(K,J) = A(K,J) *S(K)*S(J)
   70 A(J,K) = A(K,J)
      RETURN
C                   failure return
  100 IFAIL=1
      RETURN
      END
