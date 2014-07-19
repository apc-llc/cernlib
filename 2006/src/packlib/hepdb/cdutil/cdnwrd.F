*
* $Id: cdnwrd.F,v 1.1.1.1 1996/02/28 16:24:40 mclareni Exp $
*
* $Log: cdnwrd.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDNWRD(DELIM,SOURCE,IRC)
      CHARACTER*(*) SOURCE,DELIM
      IRC = 0
*
*     Find the number of words in SOURCE delimited by DELIM
*
      IR = LENOCC(SOURCE)
*     If string is non-blank, at least one word exists...
      IF(IR.EQ.0) THEN
         IRC = -1
         RETURN
      ENDIF
 
      IRC = 1
      IL = 1
      IC = 1
10    CONTINUE
      IL = INDEX(SOURCE(IC:IR),DELIM)
      IF(IL.EQ.0) GOTO 99
      IC = IC + IL
      IL = IL + 1
      IRC = IRC + 1
      IF(IC.GE.IR) GOTO 99
      GOTO 10
 
99    END
