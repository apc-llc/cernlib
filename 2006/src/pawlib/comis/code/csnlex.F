*
* $Id: csnlex.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csnlex.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 11/02/94  11.23.59  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   09/12/93
      SUBROUTINE CSNLEX(LINE,IPOS,NAME,LNAME,LEXEM)
*--------------------------------------------------
*    picks up next lexem, after call ipos point out
*    to next character after lexem
*    name contents character presentation of lexem
*    name=' ' if no more or error
*--------------------------------------------------
      CHARACTER *(*) LINE, NAME
      INTEGER IPOS,LNAME
****************************************************************
      INTEGER UNDEF,  IDENT,  INUM,  RNUM, DNUM, HOLL
     +       ,DOTDOT, CCONST, DELIM, NOMORE
      PARAMETER (UNDEF=0, IDENT=1, INUM=2, RNUM=3, DNUM=4, HOLL=5,
     +           DOTDOT=6,CCONST=7,DELIM=8,NOMORE=9 )
*****************************************************************
      CALL CSLEXM(LINE, IPOS, LEXEM, LNAME)
      IF(LNAME.GT.0)THEN
        I=IPOS+LNAME
        NAME=LINE(IPOS:I-1)
        IPOS=I
      ELSE
        NAME=' '
      ENDIF
      END
