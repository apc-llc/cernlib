*
* $Id: hexist.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hexist.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.47  by  Rene Brun
*-- Author :
      LOGICAL FUNCTION HEXIST(ID2)
*.==========>
*.           HEXIST=.TRUE. IF ID2 PRESENTLY EXIST
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
*.___________________________________________
      HEXIST=.FALSE.
      ID=ID2
      IF(LCDIR.GT.0)THEN
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.GT.0) HEXIST=.TRUE.
      ENDIF
      END
