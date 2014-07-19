*
* $Id: islwsc.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: islwsc.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/01 26/06/89  11.03.38  by  O.Couet
*-- Author :
      SUBROUTINE ISLWSC(WIDTH)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the linewidth primitive attribute.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JLWIDE (CVALUE)
*.
*.   Parameters:
*.   -----------
*.     CVALUE                     [ INTEGER; Input ]
*.
*.      - The new value for the linewidth.
*.
*.        *NOTE* : CVALUE must be in the range 0...32767.
*.      Default linewidth = 16383 (midrange)
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      INTEGER CVALUE, MIDRNG, RANGE(2)
      DATA RANGE(1), RANGE(2), MIDRNG /0, 32767, 16383/
*.______________________________________
*
      RLWSC=WIDTH
      IF (GFLAG) THEN
         IF (WIDTH.LE.1.0) THEN
            CVALUE = INT(WIDTH*FLOAT(MIDRNG))
         ELSE
            CVALUE = MIDRNG + INT(0.25*(WIDTH-1.0)*FLOAT(MIDRNG-1))
         ENDIF
         IF (CVALUE.LT.RANGE(1)) THEN
            CVALUE = RANGE(1)
         ELSE IF (CVALUE.GT.RANGE(2)) THEN
            CVALUE = RANGE(2)
         ENDIF
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JLWIDE (CVALUE)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ILWSCO)
#endif
*
      END
#endif
