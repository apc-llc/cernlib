*
* $Id: huoptc.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: huoptc.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.54  by  Rene Brun
*-- Author :
      SUBROUTINE HUOPTC(CCHOPT,CSTR,IOPT)
*.==========>
*.           Same as KERNLIB routine UOPTC, but in addition
*.           translate string CCHOPT to upper case string CHOPT.
*..=========> ( R. Brun )
      CHARACTER*(*) CCHOPT,CSTR
      CHARACTER*12 CHOPT
      DIMENSION IOPT(1)
*.___________________________________________
      CHOPT=CCHOPT
      CALL CLTOU(CHOPT)
      CALL UOPTC(CHOPT,CSTR,IOPT)
*
      RETURN
      END
