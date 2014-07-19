*
* $Id: sigmae.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sigmae.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.06/00 14/08/89  15.16.08  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIGMAE
      COMMON/SIKUIP/ CHSIGM
      CHARACTER*80 CHSIGM
      CALL SIGMA(CHSIGM)
      END
