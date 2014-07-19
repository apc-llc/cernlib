*
* $Id: hlocat.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlocat.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.50  by  Rene Brun
*-- Author :
      SUBROUTINE HLOCAT(ID1,IFIRST)
*.==========>
*.           returns the adress of hisT ID1
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*.___________________________________________
      ID=ID1
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.GT.0)THEN
         IFIRST=LQ(LTAB-IDPOS)
      ELSE
         IFIRST=0
      ENDIF
      END
