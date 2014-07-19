*
* $Id: cdtrap.F,v 1.2 1996/03/12 13:08:08 cernlib Exp $
*
* $Log: cdtrap.F,v $
* Revision 1.2  1996/03/12 13:08:08  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB_UNIX)
      SUBROUTINE CDTRAP(ISTAT)
 
      INTEGER SIGNALF
 
      EXTERNAL   CDWAKE
 
      ISTAT = SIGNALF(1,CDWAKE,-1)
 
      END
#endif
