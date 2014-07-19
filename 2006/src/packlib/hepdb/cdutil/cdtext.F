*
* $Id: cdtext.F,v 1.1.1.1 1996/02/28 16:24:42 mclareni Exp $
*
* $Log: cdtext.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:42  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTEXT(LUN,CHFILE,CHPATH,LBANK,CHOPT,IRC)
 
      CHARACTER*(*) CHFILE,CHPATH
 
#include "hepdb/cduscm.inc"
#include "hepdb/quest.inc"
#include "hepdb/hdbopts.inc"
 
      IRC = 0
 
*
*     Read text file
*
      IF(IOPTR.NE.0)     THEN
         IF(IOPTA.EQ.0) THEN
            CALL CDOPNF(LUN,CHFILE,LRECL,'FI',IRC)
            IF(IRC.NE.0) GOTO 99
         ENDIF
         CALL CDATOI(LUN,LBANK,IRC)
*
*     Write text file
*
      ELSEIF(IOPTW.NE.0) THEN
         IF(IOPTA.EQ.0) THEN
            CALL CDOPNF(LUN,CHFILE,LRECL,'FO',IRC)
            IF(IRC.NE.0) GOTO 99
         ENDIF
         CALL CDAFRI(LUN,LBANK,' ',IRC)
*
*     Print
*
      ELSEIF(IOPTP.NE.0) THEN
         CALL CDAFRI(LPRTCD,LBANK,'C',IRC)
*
*     Delete
*
      ELSEIF(IOPTD.NE.0) THEN
      ENDIF
 
   99 CONTINUE
 
      END
