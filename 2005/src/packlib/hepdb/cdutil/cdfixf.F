*
* $Id: cdfixf.F,v 1.1.1.1 1996/02/28 16:24:37 mclareni Exp $
*
* $Log: cdfixf.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:37  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFIXF(FNAME,FULLN)
#include "hepdb/cdunit.inc"
      CHARACTER*(*) FNAME,FULLN
      CHARACTER*255 CHPATH,FNAME2
      CHARACTER*20  PAFF
      PARAMETER     (MAXLEV=20)
*
*     Filename is in current directory, if full path name not specified
*
      LFNAME = LENOCC(FNAME)
      FNAME2 = FNAME
 
      IF (FNAME(1:2) .NE. '//') THEN
*
*     Add CD to start of FNAME
*
         CALL RZCDIR(CHPATH,'R')
         LC = LENOCC(CHPATH)
         IF (LFNAME .EQ. 0) THEN
            FULLN=CHPATH(1:LC)
            RETURN
         ENDIF
 
         FULLN  = CHPATH(1:LC) // '/' // FNAME2(1:LFNAME)
      ELSE
 
         FULLN = FNAME
 
      ENDIF
*
*     Now handle '..' characters
*
      CHPATH = FULLN
      LPATH  = LENOCC(CHPATH)
      CALL CDNWRD('/',CHPATH(3:LPATH),NWORDS)
      IF(NWORDS.GT.MAXLEV) THEN
         IF(LLOGCD.GE.0) WRITE(LPRTCD,*) 'CDFIXF. error - more than ',
     +      MAXLEV,' elements found in path name'
         RETURN
      ENDIF
 
      IF(LLOGCD.GE.3) WRITE(LPRTCD,*) 'CDFIXF. ',NWORDS,
     +                ' words found in ',CHPATH(3:LPATH)
 
      FULLN  = '/'
      LF     = 1
 
      DO 10 I=1,MIN(MAXLEV,NWORDS)
      PAFF   = ' '
      CALL CDWORD(PAFF,I-1,'/',CHPATH(3:LPATH),IC)
      LP   = LENOCC(PAFF)
      IF(PAFF(1:LP).NE.'..') THEN
         FULLN(LF+1:LF+LP+1) = '/'//PAFF
         IF(LLOGCD.GE.3) WRITE(LPRTCD,*)
     +      'CDFIXF. word ',I,' = ',PAFF(1:LP)
         LF = LF + LP + 1
      ELSE
         IF(LLOGCD.GE.3) WRITE(LPRTCD,*) 'CDFIXF. word .. discarded'
         LF = INDEXB(FULLN,'/') - 1
      ENDIF
10    CONTINUE
 
      FULLN(LF+1:) = ' '
 
      END
