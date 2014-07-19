*
* $Id: nampoi.F,v 1.1.1.1 1996/03/08 16:58:50 mclareni Exp $
*
* $Log: nampoi.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:50  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION NAMPOI(PNAME)
C.----------------------------------------------------------------------
C.
C.    FUNCTION CONVERTS PARTICLE NAME TO ARRAY-POINTER (BINARY SEARCH)
C.    LAST UPDATE: 20/12/88
C.
C.----------------------------------------------------------------------
#include "eurodec/ptable.inc"
#include "eurodec/namind.inc"
      CHARACTER*8 PNAME
      INDX=IDLOC(PNAME(1:7),ISTAT)
      IF (ISTAT.NE.1) THEN
        CALL ERRORD(56,PNAME,0.)
      ELSE
        NAMPOI=IPPIDX(INDX)
      ENDIF
      RETURN
      END
