*
* $Id: fmnwrd.F,v 1.1.1.1 1996/03/07 15:18:17 mclareni Exp $
*
* $Log: fmnwrd.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:17  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMNWRD(DELIM,SOURCE,IRC)
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
      IC = 1
10    CONTINUE
      IL = INDEX(SOURCE(IC:IR),DELIM)
      IF(IL.EQ.0) GOTO 99
      IC = IC + IL
      IRC = IRC + 1
      IF(IC.GE.IR) GOTO 99
      GOTO 10
 
99    END
 
