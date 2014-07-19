*
* $Id: ipfout.F,v 1.1.1.1 1996/02/14 13:11:06 mclareni Exp $
*
* $Log: ipfout.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/06 26/10/92  16.35.13  by  O.Couet
*-- Author :
      SUBROUTINE IPFOUT(Z)
*.===========>
*.
*.   Write the real Z in the PostScript file.
*.
*..==========> (O.Couet)
      CHARACTER*20 STR
*.______________________________________
*
      CALL IZRTOC(Z,STR)
      ILEN=LENOCC(STR)
      IF(STR(ILEN:ILEN).EQ.'.')THEN
         IF(ILEN.GT.1)THEN
            ILEN=ILEN-1
         ELSE
            STR='0'
            ILEN=1
         ENDIF
      ENDIF
      CALL IPPSTR(' '//STR(1:ILEN))
*
      END
