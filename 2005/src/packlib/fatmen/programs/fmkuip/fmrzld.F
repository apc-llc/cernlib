*
* $Id: fmrzld.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmrzld.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRZLD
*CMZ :          21/08/91  14.44.10  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/08/91
#include "zebra/zunit.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
#include "fatmen/fatsys.inc"
      CHARACTER*255 PATH,PATH1
      CHARACTER*80  OUTPUT,CHOPT
      LOGICAL       IOPEN
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(PATH1,LPATH)
      IF(LPATH.EQ.0) THEN
         CALL RZCDIR(PATH,'R')
      ELSE
         CALL FMFIXF(PATH1,PATH)
      ENDIF
      LPATH = LENOCC(PATH)
 
      CALL KUGETC(OUTPUT,LOUT)
 
      CALL KUGETC(CHOPT,LCH)
 
      IF(LCH.EQ.0) CHOPT = ' '
 
      IQFATS = IQPRNT
 
      IF((OUTPUT(1:LOUT).EQ.'TTY').OR.(LOUT.EQ.0)) THEN
         OUTPUT = ' '
         LWRITE = LPRTFA
      ELSE
         LWRITE = 3
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMRZLD. output will be redirected to ',OUTPUT(1:LOUT)
         CALL FAFILE(LWRITE,OUTPUT(1:LOUT),IRET)
         IF(IRET.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMRZLD. return code ',IRET,
     +      ' opening file ',OUTPUT(1:LOUT),
     +      ' - output will be sent to screen'
            OUTPUT = ' '
            LWRITE = LPRTFA
         ENDIF
      ENDIF
*
*     Call RZLDIR
*
      IQPRNT = LWRITE
 
      CALL RZLDIR(PATH(1:LPATH),CHOPT)
 
      INQUIRE(3,OPENED=IOPEN)
      IF(IOPEN) CLOSE(3)
      IQPRNT = IQFATS
      END
