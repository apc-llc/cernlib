*
* $Id: h1evli.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: h1evli.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 11/04/90  14.19.18  by  Rene Brun
*-- Author :
      SUBROUTINE H1EVLI(ID1)
*.==========>
*.           AT EDITING 1 LINE WILL CORRESPOND AT MINIMUM
*.           TO 1 ENTRY.
*..=========> ( R.Brun )
*.___________________________________________
      CALL HIDOPT(ID1,'1EVLI')
      CALL HOBSOL('H1EVLI','1EVL','AEVL')
      END
