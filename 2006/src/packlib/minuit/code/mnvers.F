*
* $Id: mnvers.F,v 1.1.1.1 1996/03/07 14:31:32 mclareni Exp $
*
* $Log: mnvers.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNVERS(CV)
#include "minuit/d506dp.inc"
CC         Returns the Minuit version in CV, char*6
CC
#include "minuit/d506cm.inc"
      CHARACTER*(*) CV
      CV = CVRSN
      RETURN
      END
