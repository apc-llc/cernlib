*
* $Id: hplsur.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplsur.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.17/08 29/09/93  11.59.27  by  O.Couet
*-- Author :
      SUBROUTINE HPLSUR(IDD,THETA,PHI,ICASE)
*.==========>
*.       3-d viewing routine
*.       surface drawn with automatic hidden line removal
*.       THETA must be between 0 and 90 degrees
*.       PHI can have any value
*..=========>
      DIMENSION PAR(2)
*._____________________________
*
      PAR(1)=THETA
      PAR(2)=PHI
      CALL HPLTAB(IDD,2,PAR,'SURF')
*
      END
