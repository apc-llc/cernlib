*
* $Id: szrm.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: szrm.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZRM(FNAME,IRC)
      CHARACTER*(*) FNAME
      CHARACTER*255 CHFORM
      LOGICAL LOPEN,IEXIST
*
*     Check if file exists
*
      LF=LENOCC(FNAME)
      INQUIRE(FILE=FNAME(1:LF),EXIST=IEXIST,
     +        FORM=CHFORM)
      IF(.NOT.IEXIST) GOTO 99
*
*     Find a unit number which is free
*
      DO 10 I=1,99
      INQUIRE(I,OPENED=LOPEN)
      IF(.NOT.LOPEN) GOTO 20
10    CONTINUE
      IRC = 1
      RETURN
20    IF(INDEX(CHFORM,'FORMATTED').EQ.0) THEN
         OPEN(I,FILE=FNAME,STATUS='OLD',ERR=99)
      ELSE
         OPEN(I,FILE=FNAME,STATUS='OLD',FORM=CHFORM,ERR=99)
      ENDIF
      CLOSE(I,STATUS='DELETE',ERR=99)
      IRC = 0
      RETURN
99    IRC = 1
      END
 
