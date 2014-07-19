*
* $Id: fmrmdc.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmrmdc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRMDC
      CHARACTER*255 FTEMP
#include "fatmen/fmpath.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatinit.inc"
*
*     Save current directory
*
      CALL RZCDIR(CDIR,'R')
      LCDIR = LENOCC(CDIR)
      CALL KUGETC(PATH,LPATH)
      IF(LPATH.EQ.0) RETURN
      CALL FMFIXF(PATH,FTEMP)
      PATH  = FTEMP
      LPATH = LENOCC(PATH)
      CALL FMRMDR(PATH(1:LPATH),IRC)
      IF (IRC .NE. 0) PRINT *,'Return code ',IRC,' from FMRMDR'
*
*     Reset current directory
*
      CALL RZCDIR(CDIR(1:LCDIR),' ')
      END
