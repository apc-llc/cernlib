*
* $Id: fmfile.F,v 1.1.1.1 1996/03/07 15:18:04 mclareni Exp $
*
* $Log: fmfile.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:04  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFILE(LUN,GENAM,CHOPT,IRC)
      CHARACTER*(*) CHOPT,GENAM
      CHARACTER*2   CHLUN
      LENTRY = 0
      WRITE(CHLUN,'(I2)') LUN
      CALL FMOPEN(GENAM,CHLUN,LENTRY,CHOPT,IRC)
      END
 
