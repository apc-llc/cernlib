*
* $Id: g901ch.F,v 1.1.1.1 1996/02/15 17:48:46 mclareni Exp $
*
* $Log: g901ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:46  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE G901CH(NREP,IDIM,X,Y,Z,T,OK)
          PARAMETER(KASES  =  30)
          REAL      X(IDIM),  Y(IDIM),  Z(IDIM),  T(IDIM)
          LOGICAL   OK
#include "kernnumt/sysdat.inc"
          IF(IDIM .LT. 3*KASES-2)  THEN
             WRITE(*,1009) IDIM, KASES
             RETURN
          ENDIF
          OK  =  .TRUE.
          NTIMES  =  NREP
  10      R  =  10.**(-KASES/2)
          DO 29     I  =  1, KASES
             R  =  9.9*R
             CALL RAN2VS(R,U,V)
             S  =  SQRT(U*U+V*V)
             IF(NRDIST(R,S) .LE. 5)  GOTO 21
                OK  =  .FALSE.
                WRITE(*,1000) NTIMES, I, R, S, U, V
  21         CALL RAN3VS(R,U,V,W)
             S  =  SQRT(U*U+V*V+W*W)
             IF(NRDIST(R,S) .LE. 10)  GOTO 24
                OK  =  .FALSE.
                WRITE(*,1000) NTIMES, I, R, S, U, V, W
  24         N  =  3*I - 2
             CALL VRAN2S(R,N,X,Y,T)
             DO 25  J  =  1, N
                S  =  SQRT(X(J)**2 + Y(J)**2)
                IF(NRDIST(R,S) .LE. 5)  GOTO 25
                   OK  =  .FALSE.
                   WRITE(*,1001) NTIMES,I,J,R,S,X(J),Y(J)
                   GOTO 26
  25            CONTINUE
  26         CALL VRAN3S(R,N,X,Y,Z,T)
             DO 27  J  =  1, N
                S  =  SQRT(X(J)**2 + Y(J)**2 + Z(J)**2)
                IF(NRDIST(R,S) .LE. 10)  GOTO 27
                   OK  =  .FALSE.
                   WRITE(*,1001) NTIMES,I,J,R,S,X(J),Y(J),Z(J)
                   GOTO 29
  27            CONTINUE
  29         CONTINUE
          NTIMES  =  NTIMES - 1
          IF(NTIMES .GT. 0)  GOTO 10
          RETURN
1000      FORMAT(2I10, 1P, 5E20.8)
1001      FORMAT(3I8,1P,5E20.8)
1009      FORMAT(' IDIM =',I6,' INSUFFICIENT FOR',I4,' CASES')
          END
