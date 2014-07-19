*
* $Id: cdexit.F,v 1.2 1996/03/12 13:08:33 cernlib Exp $
*
* $Log: cdexit.F,v $
* Revision 1.2  1996/03/12 13:08:33  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDEXIT
*
*     Clean termination - close all HEPDB files
*
      CALL CDEND(' ','AC',IRC)
*
*     and now any remaining RZ files...
*
      CALL RZCLOS(' ','A')
 
      END
