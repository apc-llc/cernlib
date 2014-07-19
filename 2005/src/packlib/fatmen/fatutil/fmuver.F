*
* $Id: fmuver.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmuver.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUVER(GENAM,LBANK,KEYS,IRC)
      CHARACTER*(*)     GENAM
#include "fatmen/fmnkeys.inc"
      DIMENSION         KEYS(LKEYFA)
      CHARACTER*8       CHROUT
#include "fatmen/fatbug.inc"
#include "fatmen/fatpara.inc"
 
      IRC = 0
      CHROUT = 'FMUVER. '
#include "fatmen/fatdummy.inc"
 
      END
