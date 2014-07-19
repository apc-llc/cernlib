*
* $Id: fmtrek.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmtrek.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMTREK
*CMZ :          21/08/91  14.44.10  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/08/91
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
#include "fatmen/fatsys.inc"
 
      CHARACTER*255 PATH,PATH1
 
      CHARACTER*80  OUTPUT
 
      LOGICAL       IOPEN
 
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(PATH1,LPATH)
      IF(LPATH.EQ.0) THEN
         CALL RZCDIR(PATH,'R')
      ELSE
         CALL FMFIXF(PATH1,PATH)
      ENDIF
      LPATH = LENOCC(PATH)
 
      CALL KUGETI(NLEVEL)
 
      CALL KUGETC(OUTPUT,LOUT)
 
      IF((OUTPUT(1:LOUT).EQ.'TTY').OR.(LOUT.EQ.0)) THEN
         OUTPUT = ' '
         LWRITE = LPRTFA
         LOUT   = 1
      ELSE
         LWRITE = 3
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMTREK. output will be redirected to ',OUTPUT(1:LOUT)
         CALL FAFILE(LWRITE,OUTPUT(1:LOUT),IRET)
         IF(IRET.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMTREK. return code ',IRET,
     +      ' opening file ',OUTPUT(1:LOUT),
     +      ' - output will be sent to screen'
            OUTPUT = ' '
            LWRITE = LPRTFA
         ENDIF
      ENDIF
*
*     Get tree
*
      WRITE(LWRITE,*) 'FMTREK. directory tree structure below ',
     +   PATH(1:LPATH),' down ',NLEVEL,' levels'
 
      CALL FATREK(PATH(1:LPATH),NLEVEL,LWRITE,IRC)
 
      WRITE(LWRITE,*)
      WRITE(LWRITE,*) IQUEST(11),' subdirectories found'
 
 
      INQUIRE(3,OPENED=IOPEN)
      IF(IOPEN) CLOSE(3)
      END
