*
* $Id: fmpaff.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmpaff.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPAFF(CHPATH,CHPAFF,NLEVEL,IRC)
*
*     Split the pathname in CHPATH into an array of component names
*     in CHPAFF
*
      CHARACTER*20 PATH
      CHARACTER*(*) CHPATH,CHPAFF(NLEVEL)
 
#include "fatmen/fatbug.inc"
      IRC    = 0
 
      LPATH  = LENOCC(CHPATH)
      CALL FMNWRD('/',CHPATH(3:LPATH),NWORDS)
      IF(NWORDS.GT.NLEVEL) IRET = -1
      IF(IDEBFA.GE.3) PRINT *,'FMPAFF. ',NWORDS,' words found in ',
     +                CHPATH(3:LPATH)
 
      J    = 0
      DO 10 I=1,MIN(NLEVEL,NWORDS)
      PATH = ' '
      CALL FMWORD(PATH,I-1,'/',CHPATH(3:LPATH),IC)
      IF(PATH.NE.'..') THEN
         J = J + 1
         CHPAFF(J) = PATH
         IF(IDEBFA.GE.3) PRINT *,'FMPAFF. word ',J,' = ',CHPAFF(J)
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMPAFF. word .. discarded'
         J = J - 1
      ENDIF
10    CONTINUE
 
      END
