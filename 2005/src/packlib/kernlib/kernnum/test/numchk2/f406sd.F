*
* $Id: f406sd.F,v 1.1.1.1 1996/02/15 17:48:42 mclareni Exp $
*
* $Log: f406sd.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:42  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      SUBROUTINE F406SD(N,M,IDIM,A)
      DOUBLE PRECISION A,ZERO,ONE,TEMP
      DIMENSION A(IDIM,N)
      DATA ZERO/0.D0/,ONE/1.D0/
C
C  SET A WELL-CONDITIONED BAND MATRIX IN A.
C
C     N        ORDER OF MATRIX.
C     M        BAND-WIDTH PARAMTER.
C     IDIM     FIRST DIMENSION OF ARRAY A.
C     A        (DOUBLE PRECISION) TWO-DIMENSIONAL ARRAY.
C
C  THE DOMINANT ELEMENTS ARE ON THE COUNTER-DIAGONALS OF THE PRINCIPAL
C  SUB-MATRICES OF ORDER M+1.
C
C  EXAMPLE .. N=8, M=2.
C
C                  . . X
C                  . X . .
C                  X . . . .
C                    . . . . X
C                      . . X . .
C                        X . . . .
C                          . . . X
C                            . X .
C
C  CALLS ... CERN PACKAGES F002 AND F003.
C
C  START.  SET A RANDOM BAND MATRIX IN A.
      MBAND=2*M+1
      MPLUS=M+1
      CALL DMRAN(N,N,-ONE,ONE,A,A(1,2),A(2,1))
      DO 2 I=1,N
         DO 1 J=1,N
            IF(IABS(I-J).GT.M) A(I,J)=ZERO
    1    CONTINUE
    2 CONTINUE
C
C  ADD MBAND TO THE COUNTER-DIAGONALS OF THE SUB-MATRICES.
      TEMP=MBAND
      NZERO=0
      DO 4 K=NZERO,N
         NU=K*MPLUS+1
         MU=MIN0(N-NU+1,MPLUS)
         IF(NU.GT.N) RETURN
         DO 3 L=1,MU
            I=NU+L-1
            J=MU+NU-L
            A(I,J)=A(I,J)+TEMP
    3    CONTINUE
    4 CONTINUE
      RETURN
      END
