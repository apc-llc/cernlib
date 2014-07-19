*
* $Id: iptlin.F,v 1.1.1.1 1996/02/14 13:11:07 mclareni Exp $
*
* $Log: iptlin.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/16 14/08/91  09.49.15  by  O.Couet
*-- Author :
      SUBROUTINE IPTLIN(CMD,IX,IY)
      CHARACTER CMD*(*)
*.===========>
*.
*.    This routine writes a drawing command to the LaTeX metafile
*.
*. _Input parameters:
*.
*.  CHARACTER CMD   : name of drawing macro
*.  INTEGER IX,IY   : coordinates
*.
*..==========> (A.Nathaniel)
#include "higz/hipost.inc"
      CHARACTER*30 STR
*.______________________________________
*
      STR=BSLASH//CMD
      L=INDEX(STR,' ')
      CALL IZITOC(IX,STR(L:))
      IF(INDEX('hrUD',CMD).EQ.0) THEN
         L=INDEX(STR,' ')
         STR(L:)=','
         CALL IZITOC(IY,STR(L+1:))
      ENDIF
      L=INDEX(STR,' ')
      STR(L:)=';'
      CALL IPPSTR(STR)
      END
