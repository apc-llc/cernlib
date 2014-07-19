*
* $Id: csnidn.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csnidn.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 11/02/94  11.34.10  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   09/12/93
      SUBROUTINE CSNIDN(LINE,IPOS,NAME,LNAME)
*--------------------------------------------
*     picks up next name
*--------------------------------------------
      CHARACTER *(*) LINE, NAME
      INTEGER IPOS,LNAME
****************************************************************
      INTEGER TOKEN
      INTEGER UNDEF,  IDENT,  INUM,  RNUM, DNUM, HOLL
     +       ,DOTDOT, CCONST, DELIM, NOMORE
      PARAMETER (UNDEF=0, IDENT=1, INUM=2, RNUM=3, DNUM=4, HOLL=5,
     +           DOTDOT=6,CCONST=7,DELIM=8,NOMORE=9 )
*****************************************************************
 1    CALL CSLEXM(LINE, IPOS, TOKEN, LNAME)
      IF(LNAME.LE.0)THEN
        NAME=' '
*       IF(LNAME.LT.0)PRINT *,'** CS-lexer error ** ', LINE(IPOS:)
      ELSEIF(TOKEN.EQ.IDENT)THEN
        I=IPOS+LNAME
        NAME=LINE(IPOS:I-1)
        IPOS=I
      ELSE
        IPOS=IPOS+LNAME
        GO TO 1
      ENDIF
      END
