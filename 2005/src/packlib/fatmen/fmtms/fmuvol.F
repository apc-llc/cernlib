*
* $Id: fmuvol.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmuvol.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUVOL(GENAM,LBANK,KEYS,LIB,MODEL,DENS,MNTTYP,LABTYP,
     +                  IRC)
      CHARACTER*(*) GENAM
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fattyp.inc"
#include "fatmen/tmsdef.inc"
*
*     Dummy user exit routine
*
      IRC = 99
      END
