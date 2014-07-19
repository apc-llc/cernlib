*
* $Id: fmfind.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmfind.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFIND(GENAME,CHLUN,LENTRY,IRC)
*
*     New FMFIND - just interface to FMOPEN
*
#include "fatmen/fatbug.inc"
      CHARACTER*(*) GENAME,CHLUN
      CHARACTER*4   CHOPT
 
      IRC = 0
      LGN = LENOCC(GENAME)
 
      IF(IDEBFA.GE.2) PRINT *,'FMFIND. enter for ',
     +   GENAME(1:LGN),' ',CHLUN,' LBANK = ',LENTRY
 
      CHOPT = 'RU'
 
      CALL FMOPEN(GENAME(1:LGN),CHLUN,LENTRY,CHOPT,IRC)
 
      END
