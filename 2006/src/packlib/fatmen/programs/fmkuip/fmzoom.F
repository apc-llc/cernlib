*
* $Id: fmzoom.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmzoom.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMZOOM
*CMZ :          21/08/91  14.44.10  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/08/91
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
#include "fatmen/fatsys.inc"
      CHARACTER*255 PATH,PATH1
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(PATH1,LPATH)
      IF(LPATH.EQ.0) THEN
         CALL RZCDIR(PATH,'R')
      ELSE
         CALL FMFIXF(PATH1,PATH)
      ENDIF
      LPATH = LENOCC(PATH)
 
      CALL FAZOOM(PATH(1:LPATH),IRC)
      CALL RZCDIR(' ','P')
 
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMZOOM. unable to find ',
     +      'lowest level directory with ',
     +      'one or more files'
      ELSE
         PRINT *,'FMZOOM. files: ',IQUEST(14)
      ENDIF
 
      END
