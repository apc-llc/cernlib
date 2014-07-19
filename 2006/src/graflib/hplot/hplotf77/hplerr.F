*
* $Id: hplerr.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplerr.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.17/05 24/08/93  18.07.32  by  O.Couet
*-- Author :
      SUBROUTINE HPLERR(XU,YU,DXU,DYU,N,CHOPT,ISYM,USIZE)
*.==========>
*.            draws error bars defined by the user
*.            symbol ISYM is drawn at the centre of the errors
*.            (Transformation NTHIST or 1 is selected
*.            according to CHOPT)
*..=========>
      CHARACTER*(*) CHOPT
      DIMENSION XU(1),YU(1),DXU(1),DYU(1)
*._____________________________
*
      CALL HPLAER(XU,YU,DXU,DXU,DYU,DYU,N,CHOPT,ISYM,USIZE)
      END
