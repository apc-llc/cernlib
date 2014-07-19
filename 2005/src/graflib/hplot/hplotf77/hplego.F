*
* $Id: hplego.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplego.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.17/08 29/09/93  11.59.27  by  O.Couet
*-- Author :
      SUBROUTINE HPLEGO(IDD,THETA,PHI)
*.==========>
*.            HPLOT 3-D viewing routine
*.            contents of cells are represented as boxes
*.            with automatic hidden lines removal
*..=========>
      DIMENSION PAR(2)
*._____________________________
*
      PAR(1)=THETA
      PAR(2)=PHI
      CALL HPLTAB(IDD,2,PAR,'LEGO')
*
      END
