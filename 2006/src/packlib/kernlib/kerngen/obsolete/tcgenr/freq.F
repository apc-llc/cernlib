*
* $Id: freq.F,v 1.1.1.1 1996/02/15 17:53:26 mclareni Exp $
*
* $Log: freq.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION FREQ (X)
C
C CERN PROGLIB# C300    FREQ            .VERSION KERNFOR  3.16  820125
C ORIG. 10/08/76 G.ERSKINE
C
C     DERIVED FROM D=ERF IN P=TCGEN WITH ERF/ERFC REMOVED.
C     20/05/80 JZ
C
C     ******************************************************************
C
C     FOR ABS(X) LE 0.47 THE BASIC FUNCTION IS ERF.  FOR ABS(X) GT 0.47
C     THE BASIC FUNCTION IS ERFC.  THE FINAL RESULT IS OBTAINED IN TERMS
C     OF THE BASIC FUNCTION AS SHOWN IN THE FOLLOWING TABLE.  A=ABS(X)
C     AND C=SQRT(2).
C
C       FUNCTION     A.LE.(0.47)                   A.GT.(0.47)
C       REQUIRED       (ALL X)          (NEGATIVE X)       (POSITIVE X)
C
C       ERF(X)         ERF(X)             ERFC(A)-1          1-ERFC(X)
C
C       ERFC(X)       1-ERF(X)            2-ERFC(A)           ERFC(X)
C
C       FREQ(X)   0.5+0.5*ERF(X/C)      0.5*ERFC(A/C)    1-0.5*ERFC(X/C)
C
C     ******************************************************************
C
      DATA CONST1/ 0.70710 67811 86548 /
C        ( CONST1= SQRT(1/2) )
C
C
      T = CONST1*X
      IF (X.GE.0.)           GO TO 31
C
C--                -VE ARGUMENT
C
      IF (X.GE.-.47)         GO TO 34
      FREQ = .5 * ERFC(-T)
      RETURN
C
C--                POSITIVE ARGUMENT
C
   31 IF (X.GT.6.0)          GO TO 38
      IF (X.GE.0.47)         GO TO 36
   34 FREQ = .5 + .5 * ERF(T)
      RETURN
C
   36 FREQ = 1. - .5 * ERFC(T)
      RETURN
C
   38 FREQ = 1.
      RETURN
      END
