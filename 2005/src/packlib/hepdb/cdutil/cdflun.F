*
* $Id: cdflun.F,v 1.1.1.1 1996/02/28 16:24:39 mclareni Exp $
*
* $Log: cdflun.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFLUN(LUN,IRC)
*
*     Free a previously allocated FORTRAN logical unit
*
#include "hepdb/hdbpara.inc"
#include "hepdb/hdblun.inc"
      IRC = 0
      DO 10 I=1,KMXLUN
      IF(MCDLUN(I).EQ.LUN) THEN
         MCDLUA(I) = -1
         GOTO 20
      ENDIF
10    CONTINUE
      IRC = -1
20    CONTINUE
#if defined(CERNLIB_VAXVMS)
      IF(IRC.NE.0) THEN
      CALL LIB$FREE_LUN(LUN)
      ENDIF
#endif
      END
