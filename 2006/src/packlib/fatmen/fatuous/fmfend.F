*
* $Id: fmfend.F,v 1.1.1.1 1996/03/07 15:18:03 mclareni Exp $
*
* $Log: fmfend.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:03  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFEND(LUN,GENAM,CHOPT,IRC)
      CHARACTER*(*) CHOPT,GENAM
      CHARACTER*2   CHLUN
      LENTRY = 0
      WRITE(CHLUN,'(I2)') LUN
      CALL FMCLOS(GENAM,CHLUN,LENTRY,CHOPT,IRC)
      END
 
