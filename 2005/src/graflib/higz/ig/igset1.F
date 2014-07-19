*
* $Id: igset1.F,v 1.1.1.1 1996/02/14 13:10:38 mclareni Exp $
*
* $Log: igset1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/12/88  10.33.39  by  O.Couet
*-- Author :
      REAL FUNCTION IGSET1(VALUE)
*.===========>
*.
*.   Underlaying for IGSET
*.
*. _Input parameters:
*.
*.  REAL VALUE : Parameter value .
*.
*..==========> (O.Couet)
      RVALUE=VALUE
      IGSET1=RVALUE
      IF(RVALUE.LT.0)THEN
         CALL IGERR('Warning: Negative value (ABS is taken)','IGSET')
         IGSET1=ABS(RVALUE)
      ENDIF
*
      END

