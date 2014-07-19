*
* $Id: fmsetu.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmsetu.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSETU(LUN,NLUN,IC)
*CMZ :          22/01/91  16.33.20  by  Jamie Shiers
*-- Author :    Jamie Shiers   22/01/91
*     Routine to set the logical units that may be used by FATMEN
*
      DIMENSION LUN(NLUN)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatlun.inc"
      IC = 0
      NUMLUN = 0
      IF(NLUN.GT.KMXLUN) IC = 1
      DO 10 I=1,MIN(NLUN,KMXLUN)
      IF((LUN(I).LE.0).OR.(LUN(I).GT.99)) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSETU. Invalid logical unit ',LUN(I),
     +   ' ignored. Logical units must be between 1-99'
         IC = 2
         GOTO 10
      ENDIF
      NUMLUN = NUMLUN + 1
      MFMLUN(NUMLUN) = LUN(I)
10    CONTINUE
      END
