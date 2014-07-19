*
* $Id: ischup.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: ischup.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/06 09/03/89  15.44.26  by  O.Couet
*-- Author :
      SUBROUTINE ISCHUP(CHUX,CHUY)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the current character plane vector text attribute.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JPLANE (CXPLAN, CYPLAN, CZPLAN)
*.
*.   Parameters:
*.   -----------
*.     CXPLAN, CYPLAN, CZPLAN     [ REAL; Input ]
*.
*.      - The character plane vector of subsequent text primitives
*.        within the currently OPEN segment.
*.
*.        *NOTE* : At least one of (CXPLAN, CYPLAN, CZPLAN) must not
*.        be equal to 0.0.
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
      RCHUX=CHUX
      RCHUY=CHUY
      IF (GFLAG) THEN
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JBASE (CHUY, -CHUX, 0.)
         CALL JPLANE (CHUX, CHUY, 0.)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF((RCHUX.EQ.0.).AND.(RCHUY.EQ.1.))THEN
         RANGLE=0.
      ELSEIF((RCHUX.EQ.-1.).AND.(RCHUY.EQ.0.))THEN
         RANGLE=90.
      ELSEIF((RCHUX.EQ.0.).AND.(RCHUY.EQ.-1.))THEN
         RANGLE=180.
      ELSEIF((RCHUX.EQ.1.).AND.(RCHUY.EQ.0.))THEN
         RANGLE=270.
      ELSE
         RANGLE=((ACOS(RCHUX/SQRT(RCHUX**2+RCHUY**2))*180.)
     +           /3.14159)-90.
         IF(RCHUY.LT.0.)RANGLE=180.-RANGLE
         IF(ABS(RANGLE).LE.0.01)RANGLE=0.
      ENDIF
      IF(GLFLAG)CALL IZATT(IANGCO)
#endif
*
      END
#endif
