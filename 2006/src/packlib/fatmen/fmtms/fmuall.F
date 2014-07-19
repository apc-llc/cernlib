*
* $Id: fmuall.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmuall.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUALL(MEDIA,CDENS,COMP,LIBRARY,POOL,LBANK,CHOPT,
     +VSN,VID,IRC)
#include "fatmen/fattyp.inc"
      CHARACTER*(*) MEDIA,LIBRARY,POOL,VSN,VID,CDENS,COMP,CHOPT
      CHARACTER*8   CHROUT
#include "fatmen/fatbug.inc"
*
*     Dummy routine
*
      IRC = 99
      CHROUT = 'FMUALL. '
#include "fatmen/fatdummy.inc"
      END
