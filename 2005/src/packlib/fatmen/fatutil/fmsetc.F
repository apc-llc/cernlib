*
* $Id: fmsetc.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmsetc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSETC(CPL,NCPL,IC)
*CMZ :          22/01/91  16.33.20  by  Jamie Shiers
*-- Author :    Jamie Shiers   22/01/91
*     Routine to set the copy level vector
*
      INTEGER CPL(NCPL)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatcpl.inc"
      IC = 0
      NUMCPL = 0
      IF(NCPL.GT.KMXCPL) IC = 1
      DO 10 I=1,MIN(NCPL,KMXCPL)
      IF(CPL(I).LE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSETC. Invalid copy level ',CPL(I),
     +   ' ignored. Location codes must be positive integers'
         IC = 2
         GOTO 10
      ENDIF
      NUMCPL = NUMCPL + 1
      MFMCPL(NUMCPL) = CPL(I)
10    CONTINUE
      END
