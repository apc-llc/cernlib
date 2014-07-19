*
* $Id: fmld.F,v 1.3 1996/04/02 10:22:59 cernlib Exp $
*
* $Log: fmld.F,v $
* Revision 1.3  1996/04/02 10:22:59  cernlib
* Murphy strikes back!  "/ *" without the space not even allowed on a
* comment line -- at least without the quotes
*
* Revision 1.2  1996/04/02 10:02:32  cernlib
* Split up "/ *" to two lines; VMS fpp took this as a comment start.
*
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLD
#include "fatmen/fmpath.inc"
      CHARACTER*255 PATH2
      CHARACTER*255 PATHI
      CHARACTER*4   CHOPT
      LOGICAL       IOPEN
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
#include "zebra/quest.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatinit.inc"
*
*     List contents of current directory
*
      CHOPT = ' '
      CALL KUGETC(PATH,LPATH)
      CALL FMNWRD('/',PATH(1:LPATH),JLEVEL)
*
*     If just a directory name with no wild cards is specified,
*     assume that we want to look down one level
*
      IF((INDEX(PATH(1:LPATH),'/').EQ.0).AND.
     +   (ICFMUL('*%()<>[]',PATH,1,LPATH).GT.LPATH)) THEN
         PATH(LPATH+1:LPATH+1) = '/'
         PATH(LPATH+2:LPATH+2) = '*'
         LPATH = LPATH + 2
      ENDIF
 
      CALL FMFIXF(PATH,PATH2)
      PATH  = PATH2
      LPATH = LENOCC(PATH)
      CALL KUGETC(OUTPUT,LOUT)
      CALL KUGETI(NLEVEL)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
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
      CALL RZCDIR(CDIR,'R')
      LCDIR = LENOCC(CDIR)
*
      IF(INDEX(CHOPT(1:LCHOPT),'H').NE.0) THEN
         WRITE(LWRITE,*) '>>> Command: LD ',
     +   'Options: ',CHOPT(1:LCHOPT)
         WRITE(LWRITE,*) '>>> Path: ',PATH(1:LPATH)
         WRITE(LWRITE,*) '>>> Current directory: ',CDIR(1:LCDIR)
      ENDIF
 
      WRITE(LWRITE,*) 'List of subdirectories...'
 
      IF(INDEX(CHOPT,'R').NE.0.AND.NLEVEL.EQ.1) NLEVEL = 99
 
      IF(JLEVEL.GT.NLEVEL) NLEVEL = JLEVEL
 
      CALL FALD(PATH(1:LPATH),NLEVEL,LWRITE,CHOPT,IRET)
 
      NDIR = IQUEST(11)
      NMAT = IQUEST(12)
 
         IF(NDIR.NE.0) THEN
            WRITE(LWRITE,9001) NDIR,NMAT
         ELSE
            WRITE(LWRITE,9002)
         ENDIF
*
*     Reset current directory
*
         CALL RZCDIR(CDIR(1:LCDIR),'U')
 
 9001    FORMAT(' Total of ',I10,' subdirectories of which ',
     +          I10,' match')
 9002    FORMAT(' No subdirectories found')
 
         INQUIRE(3,OPENED=IOPEN)
         IF(IOPEN) CLOSE(3)
         END
