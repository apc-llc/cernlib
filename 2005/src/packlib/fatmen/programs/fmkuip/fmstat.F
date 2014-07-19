*
* $Id: fmstat.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmstat.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSTAT
#include "fatmen/fmpath.inc"
      CALL KUGETC(PATH,LPATH)
      IF (PATH(1:4) .EQ. '*') THEN
         PATH = CDIR
         LPATH = LCDIR
         ENDIF
 
      CALL RZSTAT(PATH(1:LPATH),9999,' ')
      END
C
C
