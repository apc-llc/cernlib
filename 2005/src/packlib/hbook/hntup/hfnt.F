*
* $Id: hfnt.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hfnt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 27/02/92  21.23.06  by  Fons Rademakers
*-- Author :    Fons Rademakers   06/01/92
      SUBROUTINE HFNT(IDD)
*.==========>
*.
*.           Filling of a new (variable row length) n-tuple.
*.           For the data-structure description see routine HBNT.
*.
*. This routine calls HFNT1. HFNT1 checks the IDD and scans over all
*. blocks calling for each block HFNT2. HFNT2 does the actual filling.
*.
*..=========> ( A.A.Rademakers )
*
      CALL HFNT1(IDD, '*')
*
      END
