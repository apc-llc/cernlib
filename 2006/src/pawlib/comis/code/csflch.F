*
* $Id: csflch.F,v 1.2 1997/03/14 11:58:55 mclareni Exp $
*
* $Log: csflch.F,v $
* Revision 1.2  1997/03/14 11:58:55  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:34:20  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/26 17:16:19  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/02 06/04/94  15.58.04  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSFLCH(FILE)
***--------------------------
*     file name changes
***--------------------------
      CHARACTER *(*) FILE
#if (defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_PAW))&&(!defined(CERNLIB_WINNT))
      CALL CUTOL(FILE)
#endif
#if (defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX))&&(defined(CERNLIB_PAW))&&(!defined(CERNLIB_WINNT))
      LOGICAL KUQCAS
      IF(.NOT.KUQCAS())CALL CUTOL(FILE)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
*??      CHARACTER  TFILE*80
*?      CALL CLTOU(FILE)
      I=INDEX(FILE,'/')
      IF(I.EQ.0)THEN
        I=INDEX(FILE,'.')
        IF(I.NE.0)THEN
          FILE(I:I)=' '
*??          TFILE= '/' // FILE
*??          FILE=TFILE
        ENDIF
      ENDIF
#endif
*  do nothing for VAX and for Windows NT/95
      END
