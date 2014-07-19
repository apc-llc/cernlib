*
* $Id: fmextr.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fmextr.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMEXTR
      CHARACTER*255 PATHI,CHPATH
      CHARACTER*64  CHFA
      CHARACTER*36  CHOPT
#include "zebra/quest.inc"
#include "fatmen/faexcm.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fmpath.inc"
#include "fatmen/fatsys.inc"
      EXTERNAL      FAEXTR
      DIMENSION     IHDIR(4)
#include "zebra/rzdir.inc"
#include "fatmen/fatinit.inc"
 
      CALL RZCDIR(CDIR,'R')
      LCDIR = LENOCC(CDIR)
 
      CALL KUGETC(PATHI,LPATH)
      CALL FMFIXF(PATHI,PATH)
      LPATH = LENOCC(PATH)
      CALL KUGETC(CHFA,LFA)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LFA.EQ.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMEXTR. an output file name ',
     +      'must be specified'
         RETURN
      ENDIF
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      CALL FMOPTC(CHOPT,ALFNUM,IOPT)
 
      IF(IDEBFA.GE.1) PRINT *,'FMEXTR. enter for path = ',
     +   PATH(1:LPATH),' output file = ',CHFA(1:LFA),
     +   ' options = ',CHOPT(1:LCHOPT)
*
*     Open output file
*
      CALL FAFILE(LUFZFA,CHFA(1:LFA),IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMEXTR. cannot open output file'
         GOTO 99
      ENDIF
 
      CALL FZFILE(LUFZFA,0,'FAO')
 
      CALL FZLOGL(LUFZFA,MAX(IDEBFA-1,-3))
 
      NFILES = 0
      NBAD   = 0
 
      CALL FMLOOP(PATH(1:LPATH),99,FAEXTR,IRC)
 
      CALL FZENDO(LUFZFA,'TE')
 
 99   CALL RZCDIR(CDIR(1:LCDIR),' ')
      CLOSE(LUFZFA)
      IF(IDEBFA.GE.0) PRINT *,'FMEXTR. processed ',NFILES,
     +   ' files of which ',NBAD,' could not be found'
 
      END
