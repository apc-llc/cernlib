*
* $Id: iclrwk.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: iclrwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.12/11 12/04/91  16.31.42  by  O.Couet
*-- Author :
      SUBROUTINE ICLRWK(WKID,ICOFL)
*.===========>
*.
*.   Purpose:
*.   --------
*.      Force an explicit new frame action on all initalized
*.      display devices
*.
*.   Parameters:
*.   -----------
*.        No parameters
*.        JFRAME may not be called while a segment is OPEN.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiflag.inc"
      INTEGER WKID
#include "higz/di3seg.inc"
*.______________________________________
*
      IF(IGIWTY(WKID).LT.0)GOTO 20
      IF(GFLAG)THEN
         CALL IGSG(0)
         IF(WKID.GE.0)THEN
            IF (SEGOPN) THEN
               CALL JUPDAT
               CALL JCLOSE
               SEGOPN =.FALSE.
            ENDIF
            CALL JFRAME
         ENDIF
      ENDIF
  20  CONTINUE
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZCLRD
#endif
*
      END
#endif
