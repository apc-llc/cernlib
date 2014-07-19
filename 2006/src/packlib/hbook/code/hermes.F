*
* $Id: hermes.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hermes.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/13 28/09/94  08.39.36  by  Rene Brun
*-- Author :
      SUBROUTINE HERMES(IER)
*.==========>
*.           To define logical unit used for error messages
*.            If LERR.LE.0 errors are not printed by HBUG.
*..=========> ( R.Brun )
#include "hbook/hcunit.inc"
*.___________________________________________
      LERR=IER
      END
