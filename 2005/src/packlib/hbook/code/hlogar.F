*
* $Id: hlogar.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlogar.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.50  by  Rene Brun
*-- Author :
      SUBROUTINE HLOGAR(ID1)
*.==========>
*.           HISTS WILL BE PRINTED IN LOG SCALE (10) FOR CONTENTS
*..=========> ( R.Brun )
*.___________________________________________
      CALL HIDOPT(ID1,'LOGY')
      CALL HOBSOL('HLOGAR','LOGY','LINY')
      END
