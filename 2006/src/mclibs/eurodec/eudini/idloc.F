*
* $Id: idloc.F,v 1.1.1.1 1996/03/08 16:58:50 mclareni Exp $
*
* $Log: idloc.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:50  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION IDLOC(PNAME,ISTAT)
C.----------------------------------------------------------------------
C.
C.    FILL/READ CONVERSION ARRAY FOR PARTICLE NAMES USING BINARY SEARCH
C.    LAST UPDATE: 20/05/88
C.
C.----------------------------------------------------------------------
#include "eurodec/ptable.inc"
#include "eurodec/namind.inc"
      CHARACTER PNAME*8
C--
C--   BINARY SEARCH IN ARRAY OF NPA LONG
      IDMIN=0
      IDMAX=NPA+1
C--
C--   LOOP OVER SUB-DIVISIONS
   10 IF ((IDMAX-IDMIN).EQ.1) THEN
        ISTAT=0
        IDLOC=IDMIN
        RETURN
      ENDIF
      IDNXT=(IDMIN+IDMAX)/2
C--
C--   FIND POINTER...
      IF (PNAME(1:7).EQ.PNA(IPPIDX(IDNXT))(1:7)) THEN
        ISTAT=1
        IDLOC=IDNXT
        RETURN
C--
C--   ...OR MODIFY INTERVAL
      ELSEIF (PNAME(1:7).GT.PNA(IPPIDX(IDNXT))(1:7)) THEN
        IDMAX=IDNXT
      ELSE
        IDMIN=IDNXT
      ENDIF
      GOTO 10
      END
