*
* $Id: cdrzld.F,v 1.2 1996/03/12 13:09:02 cernlib Exp $
*
* $Log: cdrzld.F,v $
* Revision 1.2  1996/03/12 13:09:02  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:29  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRZLD
#include "zebra/zunit.inc"
#include "hepdb/cdunit.inc"
      CHARACTER*255 CHPATH,PATH
      CHARACTER*8   CHOPT
 
#include "hepdb/cdopen.inc"
 
      CALL KUGETC(PATH,LPATH)
      IF(LPATH.EQ.0) THEN
         CALL RZCDIR(CHPATH,'R')
      ELSE
         CALL CDFIXF(PATH,CHPATH)
      ENDIF
      LPATH = LENOCC(CHPATH)
 
      CALL KUGETC(CHOPT,LCH)
 
      IQSAVE = IQPRNT
*
*     Call RZLDIR
*
      IQPRNT = LOUTCD
 
      CALL RZLDIR(PATH(1:LPATH),CHOPT)
 
      IQPRNT = IQSAVE
 
      END
