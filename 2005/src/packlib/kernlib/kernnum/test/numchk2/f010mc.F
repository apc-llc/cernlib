*
* $Id: f010mc.F,v 1.1.1.1 1996/02/15 17:48:42 mclareni Exp $
*
* $Log: f010mc.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:42  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      REAL FUNCTION F010MC(M,N,IDIM,Q)
      COMPLEX Q(IDIM,N)
C
C  F010MC IS SET EQUAL TO THE LARGEST ABSOLUTE VALUE OF ANY ELEMENT IN
C  THE MATRIX Q.
C     M     FIRST MATHEMATICAL DIMENSION OF Q.
C     N     SECOND MATHEMATICAL DIMENSION OF Q.
C     IDIM  FIRST DIMENSION OF ARRAY Q.
C     Q     (COMPLEX) TWO-DIMENSIONAL ARRAY.
C
C  START.
      R=0.
      DO 2 I=1,M
         DO 1 J=1,N
            A=CABS(Q(I,J))
            R=AMAX1(R,A)
    1    CONTINUE
    2 CONTINUE
      F010MC=R
      RETURN
      END
          SUBROUTINE F010MG(A,R,B,OK,EQN,INV,EQINV,VCPY,VDIST,T)
          REAL                A(99), R(99), B(99)
          LOGICAL             OK, OKT
#include "kernnumt/sysdat.inc"
          REAL                T(6), E(3)
          INTEGER             NERR(5), KERR(5)
          DATA                NERR  /   0, -1, +2, +1, +1  /
          DATA                KERR  /  +1, +1, +1,  0, -1  /
          IDIM  =  1
          OK    =  .TRUE.
          DO 30     JSUB  =  1, 3
             L  =  5
             IF(JSUB .EQ. 2)  L  =  3
             DO 20  JERR  =  1, L
                N  =  NERR(JERR)
                K  =  KERR(JERR)
                CALL VCPY(1,T(1),DUMMY,A,DUMMY)
                CALL VCPY(1,T(3),DUMMY,R,DUMMY)
                CALL VCPY(1,T(5),DUMMY,B,DUMMY)
                GOTO(11, 12, 13), JSUB
  11               CALL EQN(N,A,IDIM,R,IFAIL,K,B)
                   GOTO 19
  12               CALL INV(N,A,IDIM,R,IFAIL)
                   GOTO 19
  13               CALL EQINV(N,A,IDIM,R,IFAIL,K,B)
  19            E(1)  =  VDIST(1,A,DUMMY,T(1),DUMMY)
                E(2)  =  VDIST(1,R,DUMMY,T(3),DUMMY)
                E(3)  =  VDIST(1,B,DUMMY,T(5),DUMMY)
                OKT  =  (E(1) .EQ. 0.) .AND. (E(2). EQ. 0.) .AND.
     +                  (E(3) .EQ. 0.)
                IF(.NOT. OKT) WRITE(*,1001) JSUB, JERR, E
                OK  =  OK .AND. OKT
  20            CONTINUE
  30         CONTINUE
          RETURN
1001      FORMAT( / 33H ??? ERROR DETECTED BY F010MG ..., 2I6,1P,4E12.3)
          END
