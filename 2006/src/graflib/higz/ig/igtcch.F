*
* $Id: igtcch.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igtcch.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/11 17/09/93  13.48.15  by  O.Couet
*-- Author :    O.Couet   11/05/93
      SUBROUTINE IGTCCH(CHTEXT)
*.===========>
*.
*.   This routine translate the IGTEXT control characters into
*.  IGTEXT printable characters.
*.
*. _Input/Output parameters:
*.
*.  CHARACTER CHTEXT : String to be converted
*.
*..==========> (O.Couet)
      PARAMETER (LENBUF=132)
      CHARACTER*(*) CHTEXT
      CHARACTER*(LENBUF) CHBUF
*.______________________________________
*
      ILEN  = LEN(CHTEXT)
      NCH   = LENOCC(CHTEXT)
      IF(ILEN.GT.LENBUF)GOTO 20
      LMAX  = LENBUF-3
      ICUR  = 1
      CHBUF = ' '
      DO 10 I=1,NCH
         IF(ICUR.GT.LMAX)GOTO 20
         IF     (CHTEXT(I:I).EQ.'<') THEN
            CHBUF(ICUR:) = '"L#'
         ELSEIF (CHTEXT(I:I).EQ.'>') THEN
            CHBUF(ICUR:) = '"G#'
         ELSEIF (CHTEXT(I:I).EQ.'#') THEN
            CHBUF(ICUR:) = '"F#'
         ELSEIF (CHTEXT(I:I).EQ.'$') THEN
            CHBUF(ICUR:) = '"D#'
         ELSEIF (CHTEXT(I:I).EQ.'[') THEN
            CHBUF(ICUR:) = '"M#'
         ELSEIF (CHTEXT(I:I).EQ.']') THEN
            CHBUF(ICUR:) = '"N#'
         ELSEIF (CHTEXT(I:I).EQ.'?') THEN
            CHBUF(ICUR:) = '"H#'
         ELSEIF (CHTEXT(I:I).EQ.'!') THEN
            CHBUF(ICUR:) = '"E#'
         ELSEIF (CHTEXT(I:I).EQ.'&') THEN
            CHBUF(ICUR:) = '"W#'
         ELSEIF (CHTEXT(I:I).EQ.':') THEN
            CHBUF(ICUR:) = '"J#'
         ELSEIF (CHTEXT(I:I).EQ.'%') THEN
            CHBUF(ICUR:) = '"Y#'
         ELSE
            CHBUF(ICUR:ICUR) = CHTEXT(I:I)
            ICUR = ICUR+1
            GOTO 10
         ENDIF
         ICUR = ICUR+3
   10 CONTINUE
      CHTEXT = CHBUF(1:LENOCC(CHBUF))
      RETURN
*
   20 CONTINUE
      CALL IGERR('Length of the input string is too small',
     +           'IGTCCH')
      END
