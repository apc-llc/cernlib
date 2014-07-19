*
* $Id: csrblk.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: csrblk.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 05/02/94  12.56.35  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   05/02/94
      SUBROUTINE CSRBLK(BUF,LENB)
      CHARACTER *(*) BUF
      L=LEN(BUF)
      LENB=0
      DO 1 I=1,L
        IF(BUF(I:I).EQ.' ')GO TO 2
        LENB=LENB+1
 1    CONTINUE
      RETURN
 2    DO 3 I=LENB+2,L
        IF(BUF(I:I).NE.' ')THEN
          LENB=LENB+1
          BUF(LENB:LENB)=BUF(I:I)
        ENDIF
 3    CONTINUE
      END
