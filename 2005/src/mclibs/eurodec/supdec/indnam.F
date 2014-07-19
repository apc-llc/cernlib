*
* $Id: indnam.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: indnam.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE INDNAM(INDEX,PNAME)
C.----------------------------------------------------------------------
C.
C.    ROUTINE CONVERTS PARTICLE INDEX TO PARTICLE NAME
C.    LAST UPDATE: 25/01/89
C.
C.----------------------------------------------------------------------
#include "eurodec/ptable.inc"
#include "eurodec/convrt.inc"
      CHARACTER*8 PNAME
      IPOINT=ICONV(IABS(INDEX))
      IF (IPOINT.EQ.0) CALL ERRORD(63,' ',FLOAT(INDEX))
      PNAME=PNA(IPOINT)
      IF (INDEX.NE.IPC(IPOINT)) THEN
        IF (PNAME(8:8).EQ.' ') THEN
          PNAME(8:8)='B'
        ELSEIF (PNAME(8:8).EQ.'B') THEN
          PNAME(8:8)=' '
        ELSEIF (PNAME(8:8).EQ.'+') THEN
          PNAME(8:8)='-'
        ELSEIF (PNAME(8:8).EQ.'-') THEN
          PNAME(8:8)='+'
        ENDIF
      ENDIF
      RETURN
      END
