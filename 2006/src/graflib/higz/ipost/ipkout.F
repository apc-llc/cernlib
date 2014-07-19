*
* $Id: ipkout.F,v 1.1 2006/08/31 14:15:04 couet Exp $
*
* $Log: ipkout.F,v $
* Revision 1.1  2006/08/31 14:15:04  couet
* - needed for SVG
*
*
#include "higz/pilot.h"
      SUBROUTINE IPKOUT(N)
*.===========>
*.
*.   Write integer in the SVG file with no space in front (unlike IPIOUT).
*.
*..==========> (O.Couet)
      CHARACTER*20 STR
*.______________________________________
*
      STR=' '
      CALL IZITOC(N,STR(1:))
      L=LENOCC(STR)
      CALL IPPSTR(STR(1:L))
*
      END
