*
* $Id: d105s2.F,v 1.1.1.1 1996/04/01 15:01:22 mclareni Exp $
*
* $Log: d105s2.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
C     Second function D105S2=1, therefore result= area of triangle
      FUNCTION D105S2(X,Y)
#include "gen/def64.inc"
     +    D105S2,X,Y
      D105S2=1D0
      RETURN
      END
C
