*
* $Id: szrewd.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: szrewd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZREWD(LUN,IRC)
      IRC = 0
      REWIND(LUN,ERR=99)
      RETURN
99    IRC = 1
      END
 
