*
* $Id: wordsep.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: wordsep.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE WORDSEP(STR)
C
C CERN PROGLIB# M433    WORDSEP         .VERSION KERNBIT  1.07  910903
C Author: M.Marquina 91/09/03
C
C-    Sets the CHARACTER separator used in M433 package
C
      CHARACTER*1 STR
#include "m433cs.inc"
      CHSEP =STR
      ICHSEP=1
      RETURN
      END
