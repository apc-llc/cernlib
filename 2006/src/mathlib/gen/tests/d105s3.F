*
* $Id: d105s3.F,v 1.1.1.1 1996/04/01 15:01:22 mclareni Exp $
*
* $Log: d105s3.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
C     Third function to integrate (RESULT=5/3)
      FUNCTION D105S3(X,Y)
#include "gen/def64.inc"
     +    D105S3,X,Y
      D105S3=X*Y
      RETURN
      END
