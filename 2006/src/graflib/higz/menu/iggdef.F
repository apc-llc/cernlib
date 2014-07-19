*
* $Id: iggdef.F,v 1.1.1.1 1996/02/14 13:11:15 mclareni Exp $
*
* $Log: iggdef.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:15  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.10/03 03/07/90  11.56.57  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGGDEF(CCHIN,NB,CHOUT)
*.===========>
*.
*.   This routine returns the option number NB of CHIN in CHOUT.
*.
*..==========> (O.Couet)
      CHARACTER*(*) CCHIN,CHOUT
      CHARACTER*255 CHIN
*.______________________________________
*
      CHIN=CCHIN
      IF(INDEX(CHIN,',').EQ.0)THEN
         ILEN=LENOCC(CHIN)
         CHOUT=CHIN(1:ILEN)
      ELSE
         ILEN=INDEX(CHIN,',')
         CHOUT=CHIN(1:ILEN-1)
         IF(NB.GT.1)THEN
            DO 10 I=1,NB-1
               ILEN=INDEX(CHIN,',')
               IF(ILEN.EQ.0)RETURN
               CHIN(1:ILEN)=' '
   10       CONTINUE
            ILAST=LENOCC(CHIN)
            IF(INDEX(CHIN,',').NE.0)ILAST=INDEX(CHIN,',')-1
            CHOUT=CHIN(ILEN+1:ILAST)
         ENDIF
      ENDIF
*
      END
