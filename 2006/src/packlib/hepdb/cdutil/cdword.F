*
* $Id: cdword.F,v 1.1.1.1 1996/02/28 16:24:40 mclareni Exp $
*
* $Log: cdword.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDWORD(DEST,IELEM,DELIM,SOURCE,IRC)
*
*     Routine to return a word delimited by the specified character
*     Word 0 is the text preceding the first delimiter etc.
*
      CHARACTER*(*) DEST,SOURCE
      CHARACTER*1   DELIM
*
      IF (IELEM .LT. 0) THEN
         DEST = ' '
         IRC  = 1
         RETURN
      ENDIF
*
      IF (IELEM .EQ. 0) THEN
         IEND = INDEX(SOURCE,DELIM)
         IF(IEND.EQ.0) THEN
            IEND = LENOCC(SOURCE)
         ELSE
            IEND = IEND - 1
         ENDIF
         DEST = SOURCE(1:IEND)
         RETURN
      ENDIF
*
*     Find start and end of string
*
      ISTART = 0
      IEND   = LENOCC(SOURCE)
      ILOC   = 1
*
*     Word IELEM is between IELEM and IELEM+1th occurance of
*     DELIM in SOURCE
*
      DO 10 I=1,IELEM+1
         ILOC = ISTART + 1
         ITEMP = INDEX(SOURCE(ILOC:IEND),DELIM)
         IF (I .LE. IELEM) ISTART = ISTART + ITEMP
         IF (I .EQ. IELEM+1) IEND = ISTART + ITEMP - 1
   10 CONTINUE
      IF (ITEMP .EQ. 0) IEND = LENOCC(SOURCE)
      DEST = SOURCE(ISTART+1:IEND)
      END
