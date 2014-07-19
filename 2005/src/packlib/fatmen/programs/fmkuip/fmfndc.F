*
* $Id: fmfndc.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fmfndc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFNDC
#include "fatmen/fmpath.inc"
#include "fatmen/fatsys.inc"
      CHARACTER*6  MODE
      CHARACTER*8  CHLUN
      CHARACTER*26 CHOPT
      CHARACTER*26 KOPT
#include "fatmen/fatinit.inc"
      CALL KUGETC(FILE2,LFILE2)
      CALL KUGETC(CHLUN,LCH)
      CALL FMFIXF(FILE2,FILE1)
      LFILE1 = LENOCC(FILE1)
      CALL KUGETC(CHOPT,LOPT)
      IF(LOPT.EQ.0) CHOPT = ' '
      L = 0
*
*     Build useful options string
*
      KOPT  = 'RU'
      LKOPT = 2
 
      IF(INDEX(CHOPT,'D').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'D'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'E').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'E'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'H').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'H'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'K').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'K'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'L').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'L'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'N').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'N'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'Q').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'Q'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'S').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'S'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'V').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'V'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(INDEX(CHOPT,'Y').NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'Y'
         LKOPT             = LKOPT + 1
      ENDIF
*
*     Save current directory
*
      CALL RZCDIR(CDIR,'R')
      LCDIR = LENOCC(CDIR)
      CALL FMOPEN(FILE1(1:LFILE1),CHLUN,L,KOPT,IRC)
*
*     Reset current directory
*
      CALL RZCDIR(CDIR(1:LCDIR),' ')
      END
