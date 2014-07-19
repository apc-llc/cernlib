*
* $Id: igaxi2.F,v 1.1.1.1 1996/02/14 13:10:33 mclareni Exp $
*
* $Log: igaxi2.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/01 10/09/91  11.42.17  by  O.Couet
*-- Author :
      SUBROUTINE IGAXI2(LABEL,ISTA,IEND)
*.===========>
*.
*.   Find start (ISTA) and end (IEND) of a label (LABEL).
*.
*..==========> (C.E.Vandoni)
      CHARACTER*(*) LABEL
*.______________________________________
*
      IEND=LENOCC(LABEL)
      DO 10 I=1,IEND
         KK=I
         ISTA=INDEX('1234567890-+.',LABEL(I:I))
         IF(ISTA.NE.0)GOTO 20
   10 CONTINUE
      CALL IGERR('Attempt to draw a blank label','IGAXIS')
   20 ISTA=KK
*
      END
