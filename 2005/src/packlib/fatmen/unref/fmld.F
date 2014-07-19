*
* $Id: fmld.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmld.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLD
#include "fatmen/fmpath.inc"
      PARAMETER    (MAXDIR=1000)
      COMMON/FALD/CHDIR
      CHARACTER*255 CHDIR(MAXDIR),COMAND
      CHARACTER*255 PATH2
      CHARACTER*255 PATHI
      CHARACTER*132 CARD
      CHARACTER*80  LINE
      CHARACTER*4   CHOPT
      CHARACTER*3   CHSTAT
      LOGICAL       IOPEN,IEXIST
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
#include "zebra/quest.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatinit.inc"
*
      CALL CFILL(' ',CHDIR,1,255*MAXDIR)
*
*     List contents of current directory
*
      CHOPT = ' '
      CALL KUGETC(PATH,LPATH)
      PATHI  = PATH
      LPATHI = LPATH
      CALL KUGETC(OUTPUT,LOUT)
      CALL KUGETI(NLEVEL)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      IOPTR = 0
      IF(INDEX(CHOPT,'R').NE.0) IOPTR = 1
      IF(INDEX(PATH(1:LPATH),'/').NE.0) IOPTR = 1
      CALL FMFIXF(PATH,PATH2)
      PATH  = PATH2
      LPATH = LENOCC(PATH)
 
      LWRITE = LPRTFA
*
*     Fix for strange KUIP behaviour...
*
      IF(OUTPUT(1:1).EQ.'-') THEN
         CHOPT  = OUTPUT
         LCHOPT = LOUT
         OUTPUT = 'TTY'
         LOUT   = 3
      ENDIF
 
      IF((OUTPUT(1:LOUT).NE.'TTY').OR.(LOUT.EQ.0)) THEN
         LWRITE = 3
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMLD. output will be redirected to ',OUTPUT(1:LOUT)
         CALL FAFILE(LWRITE,OUTPUT(1:LOUT),IRET)
         IF(IRET.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMLD. return code ',IRET,
     +      ' opening file ',OUTPUT(1:LOUT),
     +      ' - output will be sent to the screen'
               LWRITE = LPRTFA
               OUTPUT = 'TTY'
         ENDIF
      ELSE
         OUTPUT = 'TTY'
      ENDIF
*
*     Save current directory
*
      CALL FACDIR(CDIR,' ')
      LCDIR = LENOCC(CDIR)
*
      IF(INDEX(CHOPT(1:LCHOPT),'H').NE.0) THEN
         WRITE(LWRITE,*) '>>> Command: LD ',
     +   'Options: ',CHOPT(1:LCHOPT)
         WRITE(LWRITE,*) '>>> Path: ',PATH(1:LPATH)
         WRITE(LWRITE,*) '>>> Current directory: ',CDIR(1:LCDIR)
      ENDIF
*
*
*     Find first wild card in path name
*
      IWILD  = ICFMUL('*%(',PATH,1,LPATH)
      IF((PATH(1:LPATH).EQ.'*').OR.(IWILD.GT.LPATH)) IWILD = 0
 
      IF ((PATH(1:LPATH) .NE. 'CURRENT_DIRECTORY') .AND.
     +    (PATH(1:LPATH) .NE. '*')                 .AND.
     +    (IWILD.NE.0)                             .AND.
     +    (IOPTR .EQ. 0)) THEN
*
*     Set the current directory
*
#if !defined(CERNLIB_CZ)
         IF(IWILD.EQ.0) CALL FACDIR(PATH(1:LPATH),'U')
#endif
#if defined(CERNLIB_CZ)
         CALL CZPUTA('MESS :CD '//PATH(3:LPATH),ISTAT)
    1    CONTINUE
         CALL CZGETA(LINE,ISTAT)
         WRITE(LWRITE,9001) LINE(3:80)
         IF(LINE(1:1) .EQ. '2') GOTO 1
         CALL CZPUTA('MESS :LD ',ISTAT)
    2    CONTINUE
         CALL CZGETA(LINE,ISTAT)
         WRITE(LWRITE,9001) LINE(3:80)
         IF(LINE(1:1) .EQ. '2') GOTO 2
#endif
      ELSE
#if !defined(CERNLIB_CZ)
         CALL FACDIR(CDIR(1:LCDIR),'U')
#endif
#if defined(CERNLIB_CZ)
         CALL CZPUTA('MESS :CD '//CDIR(3:LCDIR),ISTAT)
    3    CONTINUE
         CALL CZGETA(LINE,ISTAT)
         WRITE(LWRITE,9001) LINE(3:80)
         IF(LINE(1:1) .EQ. '2') GOTO 3
#endif
      ENDIF
*
*     Read names of directories below this one
*
#if !defined(CERNLIB_CZ)
      IWIDTH = 0
      JWIDTH = 78
      IF(INDEX(CHOPT,'V').NE.0) JWIDTH = 132
*     IF((IWILD.EQ.0).AND.(IOPTR .EQ. 0)) THEN
      IF(IOPTR .EQ. 0) THEN
         CALL RZRDIR(MAXDIR,CHDIR,NDIR)
      ELSE
 
         IF((PATH(1:LPATH) .EQ. '*').OR.
     +      (PATH(1:LPATH) .EQ. 'CURRENT_DIRECTORY')) THEN
            PATH = CDIR
            LPATH = LCDIR
         ENDIF
 
         CALL FASCAN(PATH(1:LPATH),NLEVEL,LWRITE,' ',IRET)
         NDIR = IQUEST(11)
#endif
#if defined(CERNLIB_NEVER)
         ISLASH = INDEXB(PATH(1:IWILD),'/')
*
*     Find subdirectories below first branch
*
         CALL FMTREE(PATH(1:ISLASH-1),CHDIR,NLEVEL,NDIR,MAXDIR,IRET)
         IF(IRET.NE.0.AND.IDEBFA.GT.-3) PRINT *,'FMLD. Return code ',
     +   IRET,' from FMTREE'
         IF(IRET.EQ.0.AND.IDEBFA.GE.3) PRINT *,'FMLD. ',NDIR,' '
     +   //'directories found below ', PATH(1:ISLASH-1)
 
         IF(IQUEST(1).LT.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMLD. ',IQUEST(11), ' '
     +      //'directories found, only ',MAXDIR,' can be '
     +      //'displayed', ' interactively.'
         ENDIF
#endif
      ENDIF
#if defined(CERNLIB_CZ)
         CALL CZPUTA('MESS :LD',ISTAT)
    4    CONTINUE
         CALL CZGETA(LINE,ISTAT)
         WRITE(LWRITE,9001) LINE(3:80)
         IF (LINE(1:1) .EQ. '2') GOTO 4
#endif
#if !defined(CERNLIB_CZ)
*
*     Loop over directories returned
*
         IF (NDIR .GT. 0) THEN
 
            WRITE (LWRITE,9002)
 
            NMAT = 0
            DO 10 I=1,NDIR
               LDIR = LENOCC(CHDIR(I))
*
*     Match against input pattern
*
               IF(IWILD.NE.0) THEN
                  IF(IOPTR .EQ. 0) THEN
                  CALL FMATCH(CHDIR(I)(1:LDIR),PATHI(1:LPATHI),IMAT)
                  ELSE
                  CALL FMATCH(CHDIR(I)(1:LDIR),PATH(1:LPATH),IMAT)
                  ENDIF
                  IF(IMAT.NE.0) GOTO 10
               ENDIF
               NMAT = NMAT + 1
         IF((INDEX(CHOPT,'W').NE.0) .OR.
     +      (INDEX(CHOPT,'V').NE.0) .AND.
     +      (IOPTR.EQ.0)) THEN
*
*     Just display file names across the terminal...
*
            IF(IWIDTH+LDIR.GE.JWIDTH) THEN
*     flush current buffer
               WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
               IWIDTH = 0
            ENDIF
            IF(IWIDTH.EQ.0) THEN
               CARD = CHDIR(I)(1:LDIR) // ' '
            ELSE
               CARD = CARD(1:IWIDTH) // CHDIR(I)(1:LDIR) // ' '
            ENDIF
            IWIDTH = IWIDTH + LDIR + 1
         ELSE
               WRITE(LWRITE,9001) CHDIR(I)(1:LDIR)
         ENDIF
   10       CONTINUE
      IF((INDEX(CHOPT,'W').NE.0) .OR.
     +      (INDEX(CHOPT,'V').NE.0) .AND.
     +      (IOPTR.EQ.0))
     +       WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
 
 
            WRITE(LWRITE,9003) NMAT
 
         ELSE
            WRITE(LWRITE,9004)
         ENDIF
*
*     Reset current directory
*
         CALL FACDIR(CDIR(1:LCDIR),'U')
#endif
#if defined(CERNLIB_CZ)
         CALL CZPUTA('MESS :CD'//CDIR(1:LCDIR),ISTAT)
         CALL CZGETA(LINE,ISTAT)
         WRITE(LWRITE,9001) LINE(3:80)
#endif
 9001    FORMAT(1X,A)
 9002    FORMAT(' List of subdirectories...')
 9003    FORMAT(' Total of ',I10,' subdirectories')
 9004    FORMAT(' No subdirectories found')
 
         INQUIRE(3,OPENED=IOPEN)
         IF(IOPEN) CLOSE(3)
         END
 
