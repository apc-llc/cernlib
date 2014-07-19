*
* $Id: fmssrc.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmssrc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSSRC
 
#include "fatmen/fatsys.inc"
#include "fatmen/fatsat.inc"
#include "fatmen/fatinit.inc"
 
      IF(LSRCST.EQ.0) THEN
         PRINT *,'FMSSRC. source site has not been set'
      ELSE
         PRINT *,'FMSSRC. source site is ',CHSRCE(1:LSRCST)
      ENDIF
 
      END
