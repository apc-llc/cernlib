*
* $Id: cdpaff.F,v 1.1.1.1 1996/02/28 16:24:38 mclareni Exp $
*
* $Log: cdpaff.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:38  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPAFF(CHPATH,CHPAFF,NLEVEL,IRC)
*
*     Split the pathname in CHPATH into an array of component names
*     in CHPAFF
*
      CHARACTER*20 PATH
      CHARACTER*(*) CHPATH,CHPAFF(NLEVEL)
 
#include "hepdb/cdunit.inc"
      IRC    = 0
 
      LPATH  = LENOCC(CHPATH)
      CALL CDNWRD('/',CHPATH(3:LPATH),NWORDS)
      IF(NWORDS.GT.NLEVEL) IRET = -1
      IF(LLOGCD.GE.3) PRINT *,'CDPAFF. ',NWORDS,' words found in ',
     +                CHPATH(3:LPATH)
 
      J    = 0
      DO 10 I=1,MIN(NLEVEL,NWORDS)
      PATH = ' '
      CALL CDWORD(PATH,I-1,'/',CHPATH(3:LPATH),IC)
      IF(PATH.NE.'..') THEN
         J = J + 1
         CHPAFF(J) = PATH
         IF(LLOGCD.GE.3) PRINT *,'CDPAFF. word ',J,' = ',CHPAFF(J)
      ELSE
         IF(LLOGCD.GE.3) PRINT *,'CDPAFF. word .. discarded'
         J = J - 1
      ENDIF
10    CONTINUE
 
      END
