*
* $Id: fmsetk.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmsetk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSETK(KEYM,NK,CHOPT,IRC)
*CMZ :          18/10/91  10.54.50  by  Jamie Shiers
*-- Author :    Jamie Shiers   18/10/91
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fmnkeys.inc"
      CHARACTER*(*) CHOPT
      DIMENSION     KEYM(LKEYFA,NK)
#include "fatmen/fatkey.inc"
 
      IRC = 0
      LCH = LENOCC(CHOPT)
 
      IF(LCH.GT.0) CHKEY = CHOPT(1:LCH)
 
      IF(NK.GT.KMXKEY) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMSETK. key matrix limited to ',
     +      '99 rows - excess values discarded'
         IRC = 4
      ENDIF
 
      NUMKEY = MIN(KMXKEY,NK)
 
      CALL UCOPY(KEYM,MFMKEY,LKEYFA*NUMKEY)
 
      END
