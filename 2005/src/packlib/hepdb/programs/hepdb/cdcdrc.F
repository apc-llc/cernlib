*
* $Id: cdcdrc.F,v 1.2 1996/03/12 13:08:28 cernlib Exp $
*
* $Log: cdcdrc.F,v $
* Revision 1.2  1996/03/12 13:08:28  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCDRC
 
      CHARACTER*255 CHPATH
      CHARACTER*255 CHALIA
      CHARACTER*8   CHOPT
#include "hepdb/cdunit.inc"
#include "hepdb/quest.inc"
#include "hepdb/hdboptd.inc"
 
      CALL KUGETC(CHPATH,LPATH)
      CALL KUGETC(CHOPT,LOPT)
 
#include "hepdb/hdboptc.inc"
      IF(IOPTA.NE.0) THEN
#include "hepdb/hdbopta.inc"
      ENDIF
 
#include "hepdb/cdopen.inc"
 
      IF(LPATH.EQ.0) THEN
         CALL CDPWD(CHOPT)
      ELSE
*
*     Alias support
*
         IF(CHPATH(1:1).EQ.'%') THEN
            CHALIA = CHPATH(2:LPATH)
            CALL CDRALI(CHALIA,CHPATH,IRC)
            LPATH  = LENOCC(CHPATH)
         ENDIF
 
         CALL RZCDIR(CHPATH(1:LPATH),' ')
         IF(IQUEST(1).EQ.0) THEN
            CALL RZCDIR(CHPATH,'R')
            LPATH = LENOCC(CHPATH)
            WRITE(LOUTCD,*) 'CDCDRC. current working directory = ',
     +         CHPATH(1:LPATH)
         ELSE
            WRITE(LOUTCD,*) 'CDCDRC. error setting directory to ',
     +         CHPATH(1:LPATH)
            RETURN
         ENDIF
*
*     Quota
*
         IF(IOPTQ.NE.0) THEN
            WRITE(LOUTCD,*) 'Quota = ',IQUEST(10)
         ENDIF
*
*
*     Subdirectories
*
         IF(IOPTS.NE.0) THEN
            WRITE(LOUTCD,*) 'Number of subdirectories = ',IQUEST(9)
         ENDIF
*
*     Dates & Times
*
         IF(IOPTT.NE.0) THEN
            WRITE(LOUTCD,*) 'Created on ',IQUEST(14),' at ',IQUEST(15),
     +              'Modified on ',IQUEST(16),' at ',IQUEST(17)
         ENDIF
*
*     Usage
*
         IF(IOPTU.NE.0) THEN
            WRITE(LOUTCD,*) 'Number of records used = ',IQUEST(18)
            WRITE(LOUTCD,*) IQUEST(19),' megawords + ',IQUEST(20),
     +                      ' words'
         ENDIF
 
      ENDIF
 
      END
