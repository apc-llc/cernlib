*
* $Id: ipiout.F,v 1.1.1.1 1996/02/14 13:11:06 mclareni Exp $
*
* $Log: ipiout.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/12 25/10/94  13.21.32  by  O.Couet
*-- Author :
      SUBROUTINE IPIOUT(N)
*.===========>
*.
*.   Write the integer N in the PostScript file.
*.
*..==========> (O.Couet)
#include "higz/hipost.inc"
      PARAMETER (LENSTR=16)
      CHARACTER*(LENSTR)  STR
*.______________________________________
*
      STR = ' '
      WRITE (STR(2:),'(I15)') N
      I = 2
  10  IF (STR(I:I).EQ.' ') THEN
         I = I+1
         GOTO 10
      ENDIF
      ILEN = LENSTR+2-I
      IF ((ILEN+LENBUF).GT.LENBMX) THEN
         WRITE (LUNPS,'(A)',ERR=999) STRBUF(1:LENBUF)
         STRBUF = STR(I-1:)
         LENBUF = ILEN
      ELSE
         STRBUF(LENBUF+1:) = STR(I-1:)
         LENBUF            = LENBUF+ILEN
      ENDIF
*
  999 END
