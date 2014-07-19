*
* $Id: hfntb.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hfntb.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 27/02/92  21.25.32  by  Fons Rademakers
*-- Author :    Fons Rademakers   06/01/92
      SUBROUTINE HFNTB(IDD, BLKNAM)
*.==========>
*.
*.           Filling of a new (variable row length) n-tuple.
*.           For the data-structure description see routine HBNT.
*.
*. This routine calls HFNT1. HFNT1 checks the IDD and finds block BLKNAM
*. and calls HFNT2. HFNT2 does the actual filling.
*.
*..=========> ( A.A.Rademakers )
*
      CHARACTER*(*)   BLKNAM
*
      CALL HFNT1(IDD, BLKNAM)
*
      END
