*
* $Id: hqend.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqend.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  10.20.38  by  John Allison
*-- Author :    John Allison   12/09/92
      SUBROUTINE HQEND
* End operations.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
 
* Drop HQ banks and release link area.
      LHQUAD = LQ (LCDIR - 6)
      IF (LHQUAD .NE. 0) THEN
         CALL MZDROP (IHDIV, LHQUAD, ' ')
         LAREA (1) = 0
      END IF
 
      END
