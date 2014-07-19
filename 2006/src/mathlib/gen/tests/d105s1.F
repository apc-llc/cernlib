*
* $Id: d105s1.F,v 1.1.1.1 1996/04/01 15:01:22 mclareni Exp $
*
* $Log: d105s1.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
C     First function to integrate (result=158)
      FUNCTION D105S1(X,Y)
#include "gen/def64.inc"
     +    D105S1,X,Y
      D105S1=(X*X)+(Y*Y)
      RETURN
      END
C
