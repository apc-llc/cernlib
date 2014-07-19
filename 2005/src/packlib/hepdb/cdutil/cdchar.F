*
* $Id: cdchar.F,v 1.1.1.1 1996/02/28 16:24:41 mclareni Exp $
*
* $Log: cdchar.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCHAR(CHTEXT,NTEXT,LTEXT,CHPATH,LBANK,CHOPT,IRC)
 
      CHARACTER*(*) CHPATH
      CHARACTER*(*) CHTEXT(NTEXT)
 
#include "hepdb/cduscm.inc"
#include "hepdb/quest.inc"
#include "hepdb/hdbopts.inc"
 
      IRC = 0
*
*     Read text file
*
      IF(IOPTR.NE.0)     THEN
         CALL CDCTOI(CHTEXT,NTEXT,LTEXT,LBANK,IRC)
*
*     Write text file
*
      ELSEIF(IOPTW.NE.0) THEN
         CALL CDCFRI(CHTEXT,NTEXT,LTEXT,LBANK,IRC)
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
 
      END
