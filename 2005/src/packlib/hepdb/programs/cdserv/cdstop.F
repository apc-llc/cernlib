*
* $Id: cdstop.F,v 1.3 1997/03/14 17:07:41 mclareni Exp $
*
* $Log: cdstop.F,v $
* Revision 1.3  1997/03/14 17:07:41  mclareni
* WNT mods
*
* Revision 1.2.2.1  1997/01/21 11:28:54  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.2  1996/03/12 13:08:07  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSTOP
#include "hepdb/cdnamc.inc"
      CALL CDEND(' ','A',IRC)
      CALL MZEND
      END
