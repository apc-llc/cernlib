*
* $Id: cdname.F,v 1.1.1.1 1996/02/28 16:24:09 mclareni Exp $
*
* $Log: cdname.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:09  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDNAME(CHPATH,NW,CHTAG,CHOPT,IRC)
 
      CHARACTER*(*) CHPATH,CHTAG(*)
      PARAMETER     (MAXTAG=100)
      CHARACTER*8   TAGS(MAXTAG)
 
#include "hepdb/quest.inc"
#include "hepdb/cduscm.inc"
#include "hepdb/hdbopts.inc"
 
      IRC = 0
 
      IF(IOPTD.NE.0)     THEN
         CALL CDDINF( 2, CHPATH, IRC)
      ELSEIF(IOPTP.NE.0) THEN
         NW = MAXTAG
         CALL CDRNAM(CHPATH, NW, TAGS, IRC)
         IF(IRC.EQ.0) THEN
            LPATH = LENOCC(CHPATH)
            WRITE(LPRTCD,*) 'CDNAME. mnemonic names for ',
     +         CHPATH(1:LPATH)
            WRITE(LPRTCD,*) (TAGS(I),I=1,NW)
         ENDIF
      ELSEIF(IOPTR.NE.0) THEN
         CALL CDRNAM(CHPATH, NW, CHTAG, IRC)
      ELSEIF(IOPTU.NE.0) THEN
         CALL CDENAM(CHPATH, NW, CHTAG, IRC)
      ENDIF
*                                                             END CDNAME
      END
