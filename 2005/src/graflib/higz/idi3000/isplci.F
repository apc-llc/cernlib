*
* $Id: isplci.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: isplci.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.16/08 10/12/92  09.30.04  by  O.Couet
*-- Author :
      SUBROUTINE ISPLCI(ICOLI)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set color primitive attribute
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JCOLOR (CVALUE)
*.
*.   Parameters:
*.   -----------
*.
*.     CVALUE                     [ INTEGER; Input ]
*.
*.      - The new value for the color index.
*.
*.        NOTE : CVALUE must be in the range 0...32767.
*.
*.        0 - "Nornal" for the device
*.        1 -  Red
*.        2 -  Green
*.        3 -  Yellow
*.        4 -  Blue
*.        5 -  Magenta
*.        6 -  Cyan
*.        7 -  White
*.        8 -  Black
*.        9 -  Complement of the "normal" device color
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      INTEGER CVALUE, RANGE(2)
      INTEGER MAP(0:7)
      DATA MAP / 9, 0, 1, 2, 4, 3, 5, 6 /
      DATA RANGE(1), RANGE(2) /0, 32767/
*.______________________________________
*
      IPLCI=ICOLI
      IF (GFLAG) THEN
C
C        Perform GKS-->DI3000 mapping for color indices 0-7.
C
         IF((ICOLI.GE.0).AND.(ICOLI.LE.7))THEN
            CVALUE=MAP(ICOLI)
         ELSE
            CVALUE=ICOLI
         ENDIF
         IF((CVALUE.LT.RANGE(1)).OR.(CVALUE.GT.RANGE(2))) CVALUE=0
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JCOLOR (CVALUE)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IPLCCO)
#endif
*
      END
#endif
