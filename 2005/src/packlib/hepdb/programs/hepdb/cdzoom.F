*
* $Id: cdzoom.F,v 1.2 1996/03/12 13:09:14 cernlib Exp $
*
* $Log: cdzoom.F,v $
* Revision 1.2  1996/03/12 13:09:14  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:29  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDZOOM
 
#include "hepdb/quest.inc"
#include "hepdb/cdunit.inc"
      CHARACTER*255 CHPATH,PATH
 
#include "hepdb/cdopen.inc"
 
      CALL KUGETC(PATH,LPATH)
      IF(LPATH.EQ.0) THEN
         CALL RZCDIR(CHPATH,'R')
      ELSE
         CALL CDFIXF(PATH,CHPATH)
      ENDIF
      LPATH = LENOCC(CHPATH)
 
      CALL CDZOO1(CHPATH(1:LPATH),IRC)
      CALL CDPWD(' ')
 
      IF(IRC.NE.0) THEN
         IF(LLOGCD.GT.-3) WRITE(LOUTCD,*) 'CDZOOM. unable to find ',
     +      'lowest level directory with ',
     +      'one or more entries'
      ELSE
         WRITE(LOUTCD,*) 'CDZOOM. objects: ',IQUEST(14)
      ENDIF
 
      END
