*
* $Id: cdrzst.F,v 1.2 1996/03/12 13:09:04 cernlib Exp $
*
* $Log: cdrzst.F,v $
* Revision 1.2  1996/03/12 13:09:04  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRZST
 
#include "hepdb/cdunit.inc"
#include "zebra/zunit.inc"
 
      CHARACTER*255 CHPATH,PATH
      CHARACTER*8   CHOPT
      INTEGER       NLEVEL,IQSAVE
 
#include "hepdb/cdopen.inc"
 
      CALL KUGETC(PATH,LPATH)
 
      CALL CDFIXF(PATH,CHPATH)
      LPATH = LENOCC(CHPATH)
 
      CALL KUGETI(NLEVEL)
      CALL KUGETC(CHOPT,LOPT)
 
      IQSAVE = IQPRNT
 
      IQPRNT = LOUTCD
 
      CALL RZSTAT(CHPATH(1:LPATH),NLEVEL,CHOPT)
 
      IQPRINT = IQSAVE
 
      END
