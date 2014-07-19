*
* $Id: fmfixf.F,v 1.1.1.1 1996/03/07 15:18:17 mclareni Exp $
*
* $Log: fmfixf.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:17  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFIXF(FNAME,FULLN)
#include "fatmen/fmpath.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*(*) FNAME,FULLN
      CHARACTER*255 CHPATH,FNAME2
      CHARACTER*255 PAFF
      PARAMETER     (MAXLEV=20)
*
*     Filename is in current directory, if full path name not specified
*
      LFNAME = LENOCC(FNAME)
      IF(FNAME(LFNAME:LFNAME).EQ.'/') LFNAME = LFNAME - 1
      FNAME2 = FNAME(1:LFNAME)
      IF (FNAME(1:2) .NE. '//') THEN
#if !defined(CERNLIB_CZ)
*
*     Add CD to start of FNAME
*
         CALL FACDIR(CHPATH,'R')
         LC = LENOCC(CHPATH)
         IF (LFNAME .EQ. 0) THEN
            FULLN=CHPATH(1:LC)
            RETURN
         ENDIF
 
         IF(FNAME2(1:1).EQ.'/') THEN
            FULLN  = CHPATH(1:LC) // FNAME2(1:LFNAME)
         ELSE
            FULLN  = CHPATH(1:LC) // '/' // FNAME2(1:LFNAME)
         ENDIF
#endif
#if defined(CERNLIB_CZ)
         FULLN = CDIR(1:LCDIR) // '/' // FNAME2(1:LFNAME)
#endif
      ELSE
 
         FULLN = FNAME(1:LFNAME)
 
      ENDIF
*
*     Now handle '..' characters
*
      CHPATH = FULLN
      LPATH  = LENOCC(CHPATH)
      CALL FMNWRD('/',CHPATH(3:LPATH),NWORDS)
      IF(NWORDS.GT.MAXLEV) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMFIXF. error - more than ',
     +      MAXLEV,' elements found in path name'
         RETURN
      ENDIF
 
      IF(IDEBFA.GE.3) PRINT *,'FMFIXF. ',NWORDS,' words found in ',
     +                CHPATH(3:LPATH)
 
      FULLN  = '/'
      LF     = 1
 
      DO 10 I=1,MIN(MAXLEV,NWORDS)
      PAFF   = ' '
      CALL FMWORD(PAFF,I-1,'/',CHPATH(3:LPATH),IC)
      LP   = LENOCC(PAFF)
      IF(PAFF(1:LP).NE.'..') THEN
         FULLN(LF+1:LF+LP+1) = '/'//PAFF
         IF(IDEBFA.GE.3) PRINT *,'FMFIXF. word ',I,' = ',PAFF(1:LP)
         LF = LF + LP + 1
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMFIXF. word .. discarded'
         LF = INDEXB(FULLN(1:LF),'/') - 1
      ENDIF
10    CONTINUE
 
      FULLN(LF+1:) = ' '
 
      CALL CLTOU(FULLN(1:LF))
 
      END
