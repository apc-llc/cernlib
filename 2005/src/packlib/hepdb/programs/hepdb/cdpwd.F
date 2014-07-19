*
* $Id: cdpwd.F,v 1.2 1996/03/12 13:08:55 cernlib Exp $
*
* $Log: cdpwd.F,v $
* Revision 1.2  1996/03/12 13:08:55  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:28  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPWD(CHOPT)
 
#include "hepdb/cdunit.inc"
 
      CHARACTER*255 CHPATH,CHALIA
#include "hepdb/hdbopts.inc"
#include "hepdb/cdopen.inc"
 
      CALL RZCDIR(CHPATH,'R')
      LPATH = LENOCC(CHPATH)
 
      IF(IOPTA.NE.0) THEN
         CALL CDALIA(CHPATH(1:LPATH),CHALIA,'G',IRC)
         LALIA = LENOCC(CHALIA)
         IF(LALIA.EQ.0) THEN
            CHALIA = '(none)'
            LALIA  = 6
         ENDIF
         WRITE(LOUTCD,9001) CHPATH(1:LPATH),CHALIA(1:LALIA)
9001  FORMAT(' CDPWD. Current working directory = ',A,/,
     +       '                            alias = ',A)
      ELSE
         WRITE(LOUTCD,9002) CHPATH(1:LPATH)
9002  FORMAT(' CDPWD. Current working directory = ',A)
      ENDIF
 
      END
