*
* $Id: ippstr.F,v 1.1.1.1 1996/02/14 13:11:07 mclareni Exp $
*
* $Log: ippstr.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 30/11/95  10.06.40  by  O.Couet
*-- Author :
      SUBROUTINE IPPSTR(STR)
*.===========>
*.
*.  Output the string STR in the buffer STRBUF
*.
*. _Input parameters:
*.
*. CHARACTER STR : String to be buffered.
*.
*..==========> (O.Couet)
#include "higz/hipost.inc"
      CHARACTER*(*) STR
*.______________________________________
*
      LEN=LENOCC(STR)
      IF(STR(1:1).EQ.'@')THEN
         IF(LENBUF.NE.0)THEN
            WRITE(LUNPS,10000,ERR=999) STRBUF(1:LENBUF)
         ENDIF
         IF(LEN.LT.2)THEN
            STRBUF=' '
         ELSE
            STRBUF=STR(2:LEN)
         ENDIF
         LENBUF=LEN-1
         GOTO 999
      ENDIF
*
      IF(STR(LEN:LEN).EQ.'@')THEN
         IF(LENBUF.NE.0)THEN
            WRITE(LUNPS,10000,ERR=999) STRBUF(1:LENBUF)
         ENDIF
         WRITE(LUNPS,10000,ERR=999) STR(1:LEN-1)
         LENBUF=0
         GOTO 999
      ENDIF
*
      IF((LEN+LENBUF).GT.LENBMX)THEN
         WRITE(LUNPS,10000,ERR=999) STRBUF(1:LENBUF)
         STRBUF=STR(1:LEN)
         LENBUF=LEN
      ELSE
         STRBUF(LENBUF+1:) = STR(1:LEN)
         LENBUF            = LENBUF+LEN
      ENDIF
      GOTO 999
*
*====>        Fast write in the PS file
*
      ENTRY IPPSTF(ILEN,STR)
*
      IF((ILEN+LENBUF).GT.LENBMX)THEN
         WRITE (LUNPS,10000,ERR=999) STRBUF(1:LENBUF)
         STRBUF = STR(1:ILEN)
         LENBUF = ILEN
      ELSE
         STRBUF(LENBUF+1:) = STR(1:ILEN)
         LENBUF            = LENBUF+ILEN
      ENDIF
      GOTO 999
*
*====>        Fast write in the PS file without buffering
*             (Print Full STring)
*
      ENTRY IPPFST(STR)
      LEN=LENOCC(STR)
      WRITE (LUNPS,10000,ERR=999) STR(1:LEN)
*
10000 FORMAT (A)
  999 LOPRT = .TRUE.
      END
