*
* $Id: cdkhlp.F,v 1.2 1996/03/12 13:08:38 cernlib Exp $
*
* $Log: cdkhlp.F,v $
* Revision 1.2  1996/03/12 13:08:38  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKHLP
      CHARACTER*255 CHPATH,CHFILE
      CHARACTER*8   CHOPT
 
      CALL KUGETC(CHPATH,LPATH)
      CALL KUGETC(CHFILE,LFILE)
      CALL KUGETC(CHOPT, LOPT)
 
      IF(LOPT.EQ.0) THEN
         LOPT  = 1
         CHOPT = ' '
      ENDIF
 
      IOPTR = INDEX(CHOPT(1:LOPT),'R')
      IOPTW = INDEX(CHOPT(1:LOPT),'R')
      IF(IOPTR+IOPTW.NE.0) THEN
         CALL CDGLUN(LUN,IRC)
      ELSE
         LUN = 6
      ENDIF
 
      CALL CDHELP(LUN,CHFILE(1:LFILE),CHPATH(1:LPATH),
     +            CHOPT(1:LOPT),IRC)
 
      IF(LUN.NE.6) CALL CDFLUN(LUN,RC)
 
      END
