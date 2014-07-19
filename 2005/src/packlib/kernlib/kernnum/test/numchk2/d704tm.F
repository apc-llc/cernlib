*
* $Id: d704tm.F,v 1.1.1.1 1996/02/15 17:48:42 mclareni Exp $
*
* $Log: d704tm.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:42  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE D704TM(KNT,NDIM,Y)
          COMPLEX   Y(NDIM), C1, C2
#include "kernnumt/sysdat.inc"
          DATA      C1, C2    / (-10.,-10.), (+10.,+10.) /
 
          KOUNT = MAX(KNT,1)
          IF( 2**6 .GT. NDIM ) THEN
             WRITE(*,8001)
          ELSE
             DO 20  M  =  16, 6, -2
                N  =  2**M
                IF(N .GT. NDIM)  GOTO 20
                IF(KNTIME .GE. LMTIME)  GOTO 80
                CALL TIMEX(T0)
                DO 10  I = 1, KOUNT
                   CALL CVRAN(N,C1,C2,Y(1),Y(2))
  10               CONTINUE
                CALL TIMEX(T1)
                T = (T1-T0) / FLOAT(KOUNT)
                CALL TIMEX(T0)
                DO 11  I = 1, KOUNT
                   CALL CVRAN(N,C1,C2,Y(1),Y(2))
                   CALL CFFT(Y,M)
  11               CONTINUE
                CALL TIMEX(T1)
                T = (T1-T0) / FLOAT(KOUNT) - T
                KNTIME = KNTIME + 1
                WRITE( SPTIME(KNTIME), 8000 )  N, T
                KOUNT = 4*KOUNT
  20            CONTINUE
          ENDIF
          RETURN
 
  80      WRITE(*,8001)
          RETURN
 
8000      FORMAT(15HD704 CFFT   N =,I5,1PE10.3)
8001      FORMAT( / 30H SPACE FOR TIMING INSUFFICIENT)
          END
