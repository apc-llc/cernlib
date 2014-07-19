*
* $Id: xzlrm.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzlrm.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZLRM(FNAME,IRC)
      CHARACTER*(*) FNAME
      CHARACTER*255 CHFORM
      LOGICAL IEXIST
#include "cspack/czunit.inc"
*
*     Check if file exists
*
      LF=LENOCC(FNAME)
      INQUIRE(FILE=FNAME(1:LF),EXIST=IEXIST,
     +        FORM=CHFORM)
      IF(.NOT.IEXIST) GOTO 99
 
      IF(INDEX(CHFORM,'FORMATTED').EQ.0) THEN
         OPEN(LUNXZI,FILE=FNAME,STATUS='OLD',ERR=99)
      ELSE
         OPEN(LUNXZI,FILE=FNAME,STATUS='OLD',FORM=CHFORM,ERR=99)
      ENDIF
      CLOSE(LUNXZI,STATUS='DELETE',ERR=99)
      IRC = 0
      RETURN
99    IRC = 1
      END
 
