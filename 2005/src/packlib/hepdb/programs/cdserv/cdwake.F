*
* $Id: cdwake.F,v 1.2 1996/03/12 13:08:10 cernlib Exp $
*
* $Log: cdwake.F,v $
* Revision 1.2  1996/03/12 13:08:10  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDWAKE
#include "hepdb/cdwacm.inc"
 
      DATA IWAKE/0/
      SAVE IWAKE
 
      IWAKE  = IWAKE + 1
 
      IWAKCD = IWAKE
 
      END
