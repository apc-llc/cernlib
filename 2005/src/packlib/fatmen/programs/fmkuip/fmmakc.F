*
* $Id: fmmakc.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmmakc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMMAKC
#include "fatmen/fmpath.inc"
#include "fatmen/fatsys.inc"
      CHARACTER*6 MODE
      CHARACTER*8 CHLUN
#include "fatmen/fatinit.inc"
      CALL KUGETC(FILE1,LFILE1)
      CALL KUGETC(CHLUN,LCH)
      CALL FMFIXF(FILE1,FILE1)
      LFILE1 = LENOCC(FILE1)
      L = 0
*
*     Save current directory
*
      CALL RZCDIR(CDIR,'R')
      LCDIR = LENOCC(CDIR)
      CALL FMMAKE(FILE1(1:LFILE1),CHLUN,L,IRC)
*
*     Reset current directory
*
      CALL RZCDIR(CDIR(1:LCDIR),' ')
      END
C
C
