*
* $Id: ischh.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: ischh.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/06 09/03/89  15.44.26  by  O.Couet
*-- Author :
      SUBROUTINE ISCHH(CHH)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the dimensions of the character box.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JSIZE (CXSIZE, CYSIZE)
*.
*.   Parameters:
*.   -----------
*.     CXSIZE, CYSIZE             [ REAL; Input ]
*.
*.      - The new value for the character box SIZE.
*.        NOTE : Both CXSIZE anC CYSIZE must be greater than 0.0.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
*.______________________________________
*
      RCHH=CHH
      IF (GFLAG) THEN
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JSIZE (CHH,CHH)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ICHHCO)
#endif
*
      END
#endif
