*
* $Id: hdummy.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hdummy.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.47  by  Rene Brun
*-- Author :
      SUBROUTINE HDUMMY(CHROUT)
*.==========>
*.           A dummy routine has been called
*..=========> ( R.Brun )
#include "hbook/hcunit.inc"
      CHARACTER*6 CHROUT
*.___________________________________________
      WRITE(LOUT,1000)CHROUT
 1000 FORMAT(' ***** WARNING DUMMY ROUTINE ',A,' HAS BEEN CALLED ****')
      END
