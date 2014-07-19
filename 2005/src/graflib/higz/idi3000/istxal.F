*
* $Id: istxal.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: istxal.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/00 29/05/89  12.18.49  by  O.Couet
*-- Author :
      SUBROUTINE ISTXAL(JTXALH,JTXALV)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set current character string justification text attribute.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JJUST (CHORIZ, CVERT)
*.
*.   Parameters:
*.   -----------
*.     CHORIZ                     [ INTEGER; Input ]
*.
*.      - The horizontal character string justification of subsequent
*.        text primitives within the currently OPEN segment.
*.
*.        *NOTE* : CHORIZ must be in the range 1 to 3.
*.
*.     CVERT                      [ INTEGER; Input ]
*.
*.      - The vertical character string justification of subsequent
*.        text primitives within the currently OPEN segment.
*.
*.        *NOTE* : CVERT must be in the range 1 to 3.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      INTEGER CHORIZ, CVERT
#include "higz/di3seg.inc"
*.______________________________________
*
      ITXALH=JTXALH
      ITXALV=JTXALV
      IF (GFLAG) THEN
*
*        Convert GKS text alignment numbers into DI3000 text alignment numbers.
*
         IF (JTXALH.EQ.0) THEN
            CHORIZ = 1
         ELSE
            CHORIZ = JTXALH
         ENDIF
         IF (JTXALV.EQ.0) THEN
            CVERT = 1
         ELSE IF (JTXALV.EQ.1) THEN
            CVERT = 3
         ELSE IF (JTXALV.EQ.2) THEN
            CVERT = 3
         ELSE IF (JTXALV.EQ.3) THEN
            CVERT = 2
         ELSE IF (JTXALV.EQ.4) THEN
            CVERT = 1
         ELSE IF (JTXALV.EQ.5) THEN
            CVERT = 1
         ENDIF
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JJUST (CHORIZ,CVERT)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      ITXALG=10*JTXALH+JTXALV
      IF(GLFLAG)CALL IZATT(ITXACO)
#endif
*
      END
#endif
