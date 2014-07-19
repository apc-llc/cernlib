*
* $Id: siroun.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: siroun.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      FUNCTION SIROUN(X)
C
C
      I=X+SIGN(1.0,X)*.5
      SIROUN=I
      END
