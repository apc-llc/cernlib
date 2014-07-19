*
* $Id: hplsel.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplsel.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.17/07 27/09/93  10.49.15  by  O.Couet
*-- Author :    O.Couet   27/09/93
      SUBROUTINE HPLSEL(NT)
*.==========>
*.  Set the current normalization transformation NT for HIGZ and HPLOT
*..=========>
#include "hplot/hpl2.inc"
      CALL ISELNT(NT)
      NTWIN=NT
      END
