*
* $Id: d703ch.F,v 1.1.1.1 1996/02/15 17:48:42 mclareni Exp $
*
* $Log: d703ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:42  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE D703CH(NDIM,Y,NREP,OK)
          REAL      Y(NDIM)
          LOGICAL   OK
#include "kernnumt/sysdat.inc"
          DATA      MARG      /  500  /
          IRESF(RES)  =  NINT(RES/RELPR)
          OK  =  .TRUE.
          DO 50     IREP  =  1, NREP
             DO 40  M  =  1, 9
                N  =  2**M
                IF(N .GT. NDIM-2)  GOTO 900
                DO 10  I  =  1, N
                   Y(I)  =  FLOAT(I+100)
  10               CONTINUE
                CALL RFFT(Y,-M)
                CALL RFFT(Y,M)
                DO 20   I  =  1, N
                   IREL  =  IRESF(ABS(Y(I)-FLOAT(I+100)) / FLOAT(I+100))
                   IF(IREL .LE. MARG)  GOTO 20
                      WRITE(*,1000) I, IREL, MARG
                      OK  =  .FALSE.
  20               CONTINUE
  40            CONTINUE
  50         CONTINUE
          RETURN
 900      WRITE(*,9000) NDIM
          OK  =  .FALSE.
          RETURN
1000      FORMAT(37H ***** SUBROUTINE RFFT (D703) ... I =,I5,3X,
     +           7HIREL  =,I8,18H EXCEEDS MARGIN  =,I6)
9000      FORMAT( / 33H ***** SUBROUTINE RFFT (D703) ...,5X,
     +           26HSPACE AVAILABLE ... NDIM =,I6,16H IS INSUFFICIENT)
      END
