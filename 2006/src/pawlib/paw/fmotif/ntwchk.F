*
* $Id: ntwchk.F,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
*
* $Log: ntwchk.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 24/08/93  19.06.27  by  Fons Rademakers
*-- Author :    Fons Rademakers   03/05/93
      SUBROUTINE NTWCHK
*
*  Interface routine to the ntwork_check routine
*
#include "paw/pcntwk.inc"
#include "paw/pawchn.inc"
*
      CALL NTWORK_CHECK(IEDONE, CFILE, LENOCC(CFILE), CRHOST,
     +                  LENOCC(CRHOST), NTCONT)
*
      END
