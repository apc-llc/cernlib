*
* $Id: mnintr.F,v 1.1.1.1 1996/03/07 14:31:30 mclareni Exp $
*
* $Log: mnintr.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNINTR(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC       Called by user. Interfaces to MNREAD to allow user to change
CC       easily from Fortran-callable to interactive mode.
CC
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      IFLGIN = 3
      CALL MNREAD(FCN,IFLGIN,IFLGUT,FUTIL)
      WRITE (ISYSWR,'(2A/)')  ' END OF MINUIT COMMAND INPUT. ',
     +      '   RETURN TO USER PROGRAM.'
      RETURN
      END
