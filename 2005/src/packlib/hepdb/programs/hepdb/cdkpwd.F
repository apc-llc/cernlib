*
* $Id: cdkpwd.F,v 1.2 1996/03/12 13:08:40 cernlib Exp $
*
* $Log: cdkpwd.F,v $
* Revision 1.2  1996/03/12 13:08:40  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKPWD
 
      CHARACTER*8 CHOPT
 
      CALL KUGETC(CHOPT,LOPT)
      CALL CDPWD(CHOPT)
 
      END
