*
* $Id: cdsetu.F,v 1.1.1.1 1996/02/28 16:24:39 mclareni Exp $
*
* $Log: cdsetu.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSETU(LUN,NLUN,IC)
*
*     Routine to set the logical units that may be used by HEPDB
*
      DIMENSION LUN(NLUN)
#include "hepdb/hdbpara.inc"
#include "hepdb/hdblun.inc"
#include "hepdb/cdunit.inc"
      IC = 0
      NUMLUN = 0
      IF(NLUN.GT.KMXLUN) IC = 1
      DO 10 I=1,MIN(NLUN,KMXLUN)
      IF((LUN(I).LE.0).OR.(LUN(I).GT.99)) THEN
         IF(LLOGCD.GE.0) PRINT *,'CDSETU. Invalid logical unit ',LUN(I),
     +   ' ignored. Logical units must be between 1-99'
         IC = 2
         GOTO 10
      ENDIF
      NUMLUN = NUMLUN + 1
      MCDLUN(NUMLUN) = LUN(I)
10    CONTINUE
      END
