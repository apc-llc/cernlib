*
* $Id: ieupdg.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: ieupdg.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION IEUPDG(INDEX)
C.----------------------------------------------------------------------
C.
C.    FUNCTION CONVERTS EUROJET PARTON/PARTICLE CODE INTO CODE AS
C.    DEFINED BY PARTICLE DATA GROUP (PHYS. LETT. B204 (1988) 1).
C.    LAST UPDATE: 01/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/ptable.inc"
#include "eurodec/convrt.inc"
C--
C--   GET POINTER, CODE CONVERSION
      IPOINT=ICONV(ABS(INDEX))
      IF (IPOINT.EQ.0) CALL ERRORD(63,' ',FLOAT(INDEX))
      IEUPDG=IPDG(IPOINT)
C--
C--   DETERMINE SIGN...
      IF (INDEX.NE.IPC(IPOINT)) IEUPDG=-IEUPDG
      RETURN
      END
