*
* $Id: hplcap.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplcap.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.13  by  O.Couet
*-- Author :
      SUBROUTINE HPLCAP (IUNIT)
*.==========>
*.        To open/activate/deactivate/close metafile or screen
*..=========>
*._____________________________
*
*----
      CALL IGMETA(IUNIT,0)
*
      END

