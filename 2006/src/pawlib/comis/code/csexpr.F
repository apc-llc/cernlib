*
* $Id: csexpr.F,v 1.5 1997/04/24 07:04:42 couet Exp $
*
* $Log: csexpr.F,v $
* Revision 1.5  1997/04/24 07:04:42  couet
* - call now CLTOU
*
* Revision 1.4  1997/04/23 15:04:34  couet
* - reformatting of the LINE string to avoid the spaces printed in case of
* syntax error
*
* Revision 1.3  1996/06/27 16:18:18  couet
* - The string LINE was too small (function name in FUN/PLOT was limited
*   to 54 characters)
*
* Revision 1.2  1996/06/04 13:01:09  couet
* - fix to allow:
*
* Macro cc2
* Appl COMIS quit
* !file xx.f77
* quit
* fun/plot 3.*xx(x) -1 1
*
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 27/09/93  17.23.43  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSEXPR(TEXT,IADDR)
***--------------------
*       translates xyz-expr and returns its address
***--------------------
       CHARACTER *(*) TEXT
       CHARACTER *256 LINE
       CHARACTER *4   NAME
       INTEGER CSADDR
       SAVE NAME,NO
       DATA NAME/'_000'/, NO/0/
       NO = NO+1
       NO = MOD(NO,1000)
       WRITE (NAME(2:4),77) NO
 77    FORMAT(I3)
       IF (NAME(2:2).EQ.' ') NAME(2:2) = '0'
       IF (NAME(3:3).EQ.' ') NAME(3:3) = '0'
       LINE='FUNCTION '// NAME // '(X,Y,Z) '
     +    // NAME //'=' // TEXT
       IL = LENOCC(LINE)+1
       LINE(IL:IL) = '#'
       CALL CLTOU(LINE)
       CALL CSEXEC(LINE,KERR)
       IF (KERR.NE.0) THEN
         IADDR = 0
       ELSE
         IADDR = CSADDR(NAME)
         CALL CSLINK
       ENDIF
       END
