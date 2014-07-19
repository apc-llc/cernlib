*
* $Id: fmsetl.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmsetl.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSETL(LOC,NLOC,IC)
*CMZ :          22/01/91  16.33.20  by  Jamie Shiers
*-- Author :    Jamie Shiers   22/01/91
*     Routine to set the location code vector
*
      DIMENSION LOC(NLOC)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatloc.inc"
      IC = 0
      IF(NLOC.GT.KMXLOC) IC = 1
      NUMLOC = 0
      DO 10 I=1,MIN(NLOC,KMXLOC)
      IF(LOC(I).LE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSETL. Invalid location code ',
     +   LOC(I),' ignored. Location codes must be positive integers'
         IC = 2
         GOTO 10
      ENDIF
      NUMLOC = NUMLOC + 1
      MFMLOC(NUMLOC) = LOC(I)
10    CONTINUE
      END
