*
* $Id: c201s.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c201s.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C201S(N,X,F,K)
#include "gen/imp64.inc"
#include "iorc.inc"
      COMMON /FLABEL/ L
      DIMENSION X(N),F(N)
      PARAMETER (Z1 = 1)
      DATA C21,C22,C23     / 4.49D0,1.18D0,0.7D0/
      DATA C41,C42,C43,C44 /50,2.5D0,0.0078D0,19000/

      IF(L .EQ. 0) THEN
C*RC   W5=SQRT(5*Z1)
C*RC   W10=SQRT(10*Z1)
C*RC   E=EXP(Z1)
C*RC   PI=4*ATAN(Z1)
C*RC   PI1=1-1/(4*PI)
       GOTO 999
      ENDIF

      GO TO (1,2,3,4), L

    1 H=Z1/(N+1)
      TEMP=(X(K)+K*H+1)**3/2
      TEMP1=0
      IF(K .NE. 1) TEMP1=X(K-1)
      TEMP2=0
      IF(K .NE. N) TEMP2=X(K+1)
      F(K)=2*X(K)-TEMP1-TEMP2+TEMP*H**2
      GOTO 999

    2 GO TO (21,22), K
   21 F(1)=X(1)**4-6*(X(1)*X(2))**2+X(2)**4+2*X(1)**3-6*X(1)*X(2)**2+
     1     C21*(X(1)**2-X(2)**2)+C22*X(1)+C23
      GOTO 999
   22 F(2)=X(2)*(4*X(1)**3-4*X(1)*X(2)**2+6*X(1)**2-2*X(2)**2+
     1     2*C21*X(1)+C22)
      GOTO 999


    3 GO TO (31,32), K
   31 F(1)=X(1)-(4/TAN(X(2))-X(2))
      GOTO 999
   32 F(2)=X(1)-X(2)/(4-SIN(X(2)))
      GOTO 999

    4 GO TO (41,42), K
   41 F(1)=C41-X(2)*(LOG(X(1)/X(2)+SQRT(1+(X(1)/X(2))**2))+C43*X(1)/C44)
      GOTO 999
   42 F(2)=C42-(X(2)*(SQRT(1+(X(1)/X(2))**2)-1)+C43*X(1)**2/(2*C44))
  999 RETURN
      END
