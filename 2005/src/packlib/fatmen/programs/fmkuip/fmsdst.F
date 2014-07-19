*
* $Id: fmsdst.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmsdst.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSDST
 
#include "fatmen/fatsys.inc"
#include "fatmen/fatsat.inc"
#include "fatmen/fatinit.inc"
 
      IF(LDSTST.EQ.0) THEN
         PRINT *,'FMSDST. destination site has not been set'
      ELSE
         PRINT *,'FMSDST. destination site is ',CHDEST(1:LDSTST)
      ENDIF
 
      END
