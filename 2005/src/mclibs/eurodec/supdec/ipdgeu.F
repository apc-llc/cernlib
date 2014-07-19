*
* $Id: ipdgeu.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: ipdgeu.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION IPDGEU(INDEX)
C.----------------------------------------------------------------------
C.
C.    FUNCTION CONVERTS PARTON/PARTICLE CODE AS DEFINED BY PARTICLE
C.    DATA GROUP (PHYS. LETT. B204 (1988) 1) INTO EUROJET CODE.
C.    LAST UPDATE: 01/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/ptable.inc"
C--
C--   SEARCH PARTICLE POSITION IN TABLE, CODE CONVERSION
      DO 10 IPOINT=1,NPMAX
        IF (ABS(IPDG(IPOINT)).EQ.ABS(INDEX)) THEN
          IPDGEU=IPC(IPOINT)
C--
C--   DETERMINE SIGN...
          IF (INDEX.NE.IPDG(IPOINT)) IPDGEU=-IPDGEU
          RETURN
        ENDIF
   10 CONTINUE
      CALL ERRORD(63,' ',FLOAT(ABS(INDEX)))
      IPDGEU=-1
      RETURN
      END
