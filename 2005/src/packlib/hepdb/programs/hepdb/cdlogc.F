*
* $Id: cdlogc.F,v 1.2 1996/03/12 13:08:45 cernlib Exp $
*
* $Log: cdlogc.F,v $
* Revision 1.2  1996/03/12 13:08:45  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:28  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLOGC
 
      CHARACTER*255 CHPATH
      CHARACTER*8   CHOPT
 
      CALL KUGETC(CHPATH,LPATH)
      CALL KUGETI(LOGL)
      CALL KUGETC(CHOPT,LOPT)
 
      IF(LPATH.EQ.0) THEN
         CHPATH = ' '
         LPATH  = 1
      ENDIF
 
      IF(LOPT.EQ.0) THEN
         CHOPT = ' '
         LOPT  = 1
      ENDIF
 
      CALL CDLOGL(CHPATH(1:LPATH),LOGL,CHOPT(1:LOPT),IRC)
 
 
      END
