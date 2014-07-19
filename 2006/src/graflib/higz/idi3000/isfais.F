*
* $Id: isfais.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: isfais.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.08/00 29/09/89  10.21.04  by  O.Couet
*-- Author :
      SUBROUTINE ISFAIS(INTS)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the interior style attribute.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JPINTR (CVALUE)
*.
*.   Parameters:
*.   -----------
*.     CVALUE                     [ INTEGER; Input ]
*.
*.      - The new value for the polygon interior style.
*.
*.        NOTE : CVALUE must be in the range 0...32767.
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
      IF((INTS.LT.0).OR.(INTS.GT.3))THEN
         CALL IGERR('Fill area interior style must be > -1 and < 4'
     +,             'ISFAIS')
         RETURN
      ENDIF
      IFAIS=INTS
      IF (GFLAG) THEN
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JPEDGE(0)
         IF (INTS.EQ.0) THEN
            CALL JPINTR(0)
         ELSE
            CALL JPINTR(1)
            IF (INTS.EQ.1) THEN
               CALL ISFASI(47)
            ELSE
               CALL ISFASI(IFASI)
            ENDIF
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IFAICO)
#endif
*
      END
#endif
