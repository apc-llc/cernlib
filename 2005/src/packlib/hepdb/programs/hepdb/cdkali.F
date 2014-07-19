*
* $Id: cdkali.F,v 1.2 1996/03/12 13:08:37 cernlib Exp $
*
* $Log: cdkali.F,v $
* Revision 1.2  1996/03/12 13:08:37  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKALI
 
      CHARACTER*255 CHPATH
      CHARACTER*8   CHOPT,CHALIA
 
      CALL KUGETC(CHPATH,LPATH)
      CALL KUGETC(CHALIA,LALIA)
      CALL KUGETC(CHOPT,LOPT)
 
      IF(LOPT.GT.1) THEN
         PRINT *,'CDKALI. error - please specify only 1 option'
         RETURN
      ENDIF
 
      IF(CHOPT.EQ.'R'.AND.LPATH.GT.0.AND.LALIA.EQ.0) THEN
         CHALIA = CHPATH(1:LPATH)
         LALIA  = LPATH
         CHPATH = ' '
      ENDIF
 
      CALL CDALIA(CHPATH,CHALIA,CHOPT,IRC)
 
      IF(CHOPT(1:LOPT).EQ.'R') THEN
         PRINT *,'CDKALI. equivalence name for ',CHALIA(1:LALIA),
     +      ' is ',CHPATH(1:LENOCC(CHPATH))
      ENDIF
 
      END
