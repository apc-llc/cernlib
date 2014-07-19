*
* $Id: cdhelp.F,v 1.1.1.1 1996/02/28 16:24:09 mclareni Exp $
*
* $Log: cdhelp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:09  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDHELP(LUN,CHFILE,CHPATH,CHOPT,IRC)
*
*     Help file manipulation
*
      CHARACTER*(*) CHFILE,CHPATH
#include "hepdb/cduscm.inc"
#include "hepdb/quest.inc"
#include "hepdb/hdbopts.inc"
 
      IRC = 0
 
      IF(IOPTD.NE.0)     THEN
*
*     Delete help information
*
         CALL CDDINF( 1, CHPATH, IRC)
      ELSEIF(IOPTP.NE.0) THEN
*
*     Print help information
*
         CALL CDRHLP(CHPATH, LUN, 'C', IRC)
      ELSEIF(IOPTR.NE.0) THEN
*
*     Read help information from LUN and write to database
*
         IF(IOPTA.EQ.0) THEN
*
*     File already open?
*
            CALL CDOPNF(LUN,CHFILE,LRECL,'FI',IRC)
            IF(IRC.NE.0) GOTO 99
         ENDIF
         CALL CDEHLP(CHPATH, LUN, IRC)
      ELSEIF(IOPTW.NE.0) THEN
*
*     Read help information from database and write to LUN
*
         IF(IOPTA.EQ.0) THEN
*
*     File already open?
*
            CALL CDOPNF(LUN,CHFILE,LRECL,'FO',IRC)
            IF(IRC.NE.0) GOTO 99
         ENDIF
         CALL CDRHLP(CHPATH,LUN,' ',IRC)
      ENDIF
 
99    CONTINUE
*                                                             END CDHELP
      END
