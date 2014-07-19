*
* $Id: fmldir.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmldir.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLDIR(CHPATH,FILES,NFOUND,MAXFIL,ICONT,IRC)
#include "zebra/quest.inc"
#include "fatmen/fatbug.inc"
*     Maximum number of subdirectories at any level
      PARAMETER     (MAXDIR=1000)
*     Maximum number of directories on the stack
      PARAMETER     (MAXSTK=1000)
      CHARACTER*20  SUBDIR(MAXDIR),MATCH
      CHARACTER*(*) CHPATH
      CHARACTER*255 PATH,CHDIR
      COMMON/FALDIR/STACK
      CHARACTER*255 STACK(MAXSTK),FILES(MAXFIL)
      PARAMETER     (NCH=20)
      PARAMETER     (MAXLEV=20)
      CHARACTER*20  DIRNAM(MAXLEV)
      CHARACTER*20  CHWORD
      SAVE          NSTACK
*
*     IRC is set to -1 if more directories found than can be returned
*     IRC is set to -2 if # directories at any level exceeds MAXDIR
*     in FILES(MAXFIL)
*     FMLDIR should then be called again with ICONT^=0 and the next
*     batch of directories will be returned.
*
      NFOUND = 0
      IRC    = 0
      LPATH  = LENOCC(CHPATH)
      CALL CLTOU(CHPATH)
      IF(ICONT.EQ.0) THEN
         NSTACK = 0
*
*     Split input path name into its component pieces
*     (this is the opposite of RZPAFF which glues them together)
*
         CALL FMPAFF(CHPATH(1:LPATH),DIRNAM,MAXLEV,IRET)
      ELSE
         GOTO 30
      ENDIF
*
*     Find first wild card in generic name
*
      IF(IDEBFA.GE.3)
     +PRINT *,'FMLDIR. enter for PATH = ',CHPATH(1:LPATH),
     +   ' MAXDIR = ',MAXFIL
      IWILD  = ICFMUL('*%(<>',CHPATH,1,LPATH)
*
*     No wild cards found - just return directories below CHPATH
*
      IF(IWILD.GT.LPATH) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMLDIR. look for subdirectories '
     +   //'below ', CHPATH(1:LPATH)
         CALL FACDIR(CHPATH(1:LPATH),' ')
         IF(IQUEST(1).NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMLDIR. cannot set directory ',
     +         'to ',CHPATH(1:LPATH)
            IRC = IQUEST(1)
            RETURN
         ENDIF
         CALL CFILL(' ',SUBDIR,1,20*MAXDIR)
         CALL RZRDIR(MAXDIR,SUBDIR,NFOUND)
         IF(IDEBFA.GE.3) PRINT *,'FMLDIR. ',NFOUND,' subdirectories '
     +   //'found'
         DO 10 I=1,NFOUND
            FILES(I) = ' '
            PATH   = CHPATH(1:LPATH)//'/'
     +               //SUBDIR(I)(1:LENOCC(SUBDIR(I)))
            FILES(I) = PATH(1:LENOCC(PATH))
   10    CONTINUE
         RETURN
      ENDIF
 
      ISLASH = INDEXB(CHPATH(1:IWILD),'/')
      JSLASH = INDEX(CHPATH(IWILD:LPATH),'/') + IWILD -2
*
*     Find subdirectories below first branch
*
      IF(IDEBFA.GE.3)
     +PRINT *,'FMLDIR. look for subdirectories below ',
     +CHPATH(1:ISLASH-1)
      CALL FACDIR(CHPATH(1:ISLASH-1),' ')
      IF(IQUEST(1).NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMLDIR. cannot set directory ',
     +      'to ',CHPATH(1:ISLASH-1)
         IRC = IQUEST(1)
         RETURN
      ENDIF
      CALL CFILL(' ',SUBDIR,1,20*MAXDIR)
      CALL RZRDIR(MAXDIR,SUBDIR,NSDIR)
      IF(IDEBFA.GE.2)
     +PRINT *,'FMLDIR. ',NSDIR,' subdirectories found below ',
     +CHPATH(1:ISLASH-1)
*
*     Perform wild-card matching
*
      CALL FMNWRD('/',CHPATH(3:LPATH),NWORDS)
      LDIR = LENOCC(DIRNAM(NWORDS))
      IBRA = ICFMUL('<>',DIRNAM(NWORDS),1,LDIR)
      IF(IBRA.GT.LDIR) IBRA = 0
 
      IF(IBRA.NE.0) THEN
         CALL FMMANY(DIRNAM(NWORDS)(1:LDIR),SUBDIR,NSDIR,NMATCH,IRET)
         IF(NMATCH.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMLDIR. no directories match ',
     +      'selection ',DIRNAM(NWORDS)(1:LDIR)
            IRC = 1
            RETURN
         ELSE
            IF(IDEBFA.GE.3) PRINT *,'FMLDIR. selected ',
     +         SUBDIR(NMATCH)
         ENDIF
         NLOW  = NMATCH
         NHIGH = NMATCH
         NSDIR = 1
      ELSE
         NLOW = 1
         NHIGH = NSDIR
      ENDIF
 
      DO 20 I=NLOW,NHIGH
         FILES(I) = ' '
         PATH = CHPATH(1:ISLASH)//SUBDIR(I)(1:LENOCC(SUBDIR(I)))
         LP = LENOCC(PATH)
*
*     Compare the current subdirectory name with the corresponding
*     component of the input file name
*
      CALL FMNWRD('/',PATH(3:LP),NWUDS)
      IF(NWUDS.LE.NWORDS) THEN
      CALL FMATCH(SUBDIR(I)(1:LENOCC(SUBDIR(I))),DIRNAM(NWUDS)
     +            (1:LENOCC(DIRNAM(NWUDS))),IMAT)
      IF(IMAT.NE.0) GOTO 20
      ENDIF
         NSTACK = NSTACK + 1
         STACK(NSTACK) = PATH(1:LP)
*
*     Only move subdirectory to output array if complete path name
*     matches.
*
         CALL FMATCH(PATH(1:LP),CHPATH(1:LPATH),IRET)
         IF(IRET.EQ.0) THEN
            NFOUND = NFOUND + 1
            FILES(NFOUND) = PATH(1:LP)
         ENDIF
   20 CONTINUE
      PATH = ' '
   30 CONTINUE
      IF(NSTACK.EQ.0) GOTO 50
      CHDIR = STACK(NSTACK)
      NSTACK = NSTACK - 1
      LCHDIR = LENOCC(CHDIR)
      CALL FACDIR(CHDIR(1:LCHDIR),' ')
      IF(IQUEST(1).NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMLDIR. cannot set directory ',
     +      'to ',CHDIR(1:LCHDIR)
         IRC = IQUEST(1)
         RETURN
      ENDIF
*
*     For each subdirectory, find subdirectories...
*
      CALL CFILL(' ',SUBDIR,1,20*MAXDIR)
      CALL RZRDIR(MAXDIR,SUBDIR,NSDIR)
      IF(NSDIR.EQ.0) GOTO 30
*
*     Perform wild-card matching at this level
*
      CALL FMNWRD('/',CHDIR(3:LENOCC(CHDIR)),NWORDS)
      LDIR = LENOCC(DIRNAM(NWORDS+1))
      IBRA = ICFMUL('<>',DIRNAM(NWORDS+1),1,LDIR)
      IF(IBRA.GT.LDIR) IBRA = 0
      IF(IBRA.NE.0) THEN
         CALL FMMANY(DIRNAM(NWORDS+1)(1:LDIR),SUBDIR,NSDIR,NMATCH,
     +   IRET)
         IF(NMATCH.EQ.0) GOTO 30
         NLOW = NMATCH
         NHIGH = NMATCH
         NSDIR = 1
      ELSE
         NLOW = 1
         NHIGH = NSDIR
      ENDIF
*
*     Check if we can accept any more directories
*     If not, leave last entry on the stack and return
*
      IF(NSDIR.GT.MAXFIL) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMLDIR. number of subdirectories ',
     +      '(',NSDIR,')',' exceeds MAXDIR ',
     +      '(',MAXFIL,')',' directory = ',CHDIR(1:LCHDIR)
         IRC = -2
         RETURN
      ENDIF
 
      IF(NFOUND+NSDIR.GT.MAXFIL) THEN
         IF(IDEBFA.GE.2) PRINT *, 'FMLDIR. maximum number of '
     +   //'subdirectories reached'
         NSTACK = NSTACK + 1
         IRC = -1
         ICONT = 1
         RETURN
      ENDIF
 
      IF(NSTACK+NSDIR.GT.MAXSTK) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMLDIR. Stack overflow'
         IRC = 1
         RETURN
      ENDIF
 
      DO 40 I=NLOW,NHIGH
         PATH = CHDIR(1:LENOCC(CHDIR)) // '/' // SUBDIR(I)(1:
     +   LENOCC(SUBDIR(I)))
         LP = LENOCC(PATH)
         IF(IDEBFA.GE.3) PRINT *,'FMLDIR. processing directory ',
     +   PATH(1:LP)
         NSTACK = NSTACK + 1
         STACK(NSTACK) = PATH(1:LP)
         IF(IBRA.EQ.0) THEN
            CALL FMATCH(PATH(1:LP),CHPATH(1:LPATH),IRET)
            IF(IRET.NE.0) GOTO 40
         ENDIF
         NFOUND = NFOUND + 1
         FILES(NFOUND) = PATH(1:LP)
   40 CONTINUE
      GOTO 30
   50 CONTINUE
      END
