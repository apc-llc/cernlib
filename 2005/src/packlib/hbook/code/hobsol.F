*
* $Id: hobsol.F,v 1.1.1.1 1996/01/16 17:07:44 mclareni Exp $
*
* $Log: hobsol.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.50  by  Rene Brun
*-- Author :
      SUBROUTINE HOBSOL(CHROUT,CH1,CH2)
*.==========>
*.           To inform new users of obsolete routines
*.           and of the equivalent
*..=========> ( R.Brun )
#include "hbook/hcunit.inc"
      CHARACTER*6 CHROUT
      CHARACTER*4 CH1,CH2
*.___________________________________________
      WRITE(LOUT,1000)CHROUT,CH1,CH2
 1000 FORMAT(' ***** OBSOLETE ROUTINE ',A,' HAS BEEN CALLED',/,
     +       ' ***** USE   CALL HIDOPT(ID,"',A,'"/"',A,'")   INSTEAD')
      END
