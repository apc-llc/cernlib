*
* $Id: funct.F,v 1.1.1.1 1996/04/01 15:01:26 mclareni Exp $
*
* $Log: funct.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:26  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      FUNCTION FUNCT(X)
C-----FUNCT CALCULATES THEORETICAL FUNCTION Y AT EACH DATA POINT
      COMMON/A/A(100)/ENDFLG/ENDFLG,NA,INDFLG(5)
      COMPLEX B,C
      EQUIVALENCE (B,A(1)),(C,A(3))
      IF(INDFLG(3)) 2,1,2
C-----FUNCT FOR FUMILI
    1 FUNCT=(A(1)/(SQRT(6.2832)*A(2)))*EXP(-((X-A(3))**2)/
     1(2.*(A(2)**2)))
      RETURN
C-----FUNCT FOR LIKELM
    2 FUNCT=(EXP(A(1)*X)+AIMAG(CEXP(B*X+C)))/
     1   ((EXP(A(1))-1.)/A(1)+AIMAG((CEXP(B+C)-CEXP(C))/B))
      RETURN
      END
