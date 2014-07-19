*
* $Id: fmpwd.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmpwd.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPWD
#include "fatmen/fmpath.inc"
      COMMON/QUEST/ IQUEST(100)
      CHARACTER*80  LINE
#if !defined(CERNLIB_CZ)
      CALL RZCDIR(' ','P')
      IF (IQUEST(1) .NE. 0) PRINT *,'Return code ',IQUEST(1),
     +' from RZCDIR'
#endif
#if defined(CERNLIB_CZ)
      CALL CZPUTA('MESS :CD',ISTAT)
      CALL CZGETA(LINE,ISTAT)
      PRINT *,LINE(3:80)
#endif
      END
C
