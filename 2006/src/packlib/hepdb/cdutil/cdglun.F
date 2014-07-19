*
* $Id: cdglun.F,v 1.1.1.1 1996/02/28 16:24:39 mclareni Exp $
*
* $Log: cdglun.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDGLUN(LUN,IRC)
#include "hepdb/hdbpara.inc"
#include "hepdb/hdblun.inc"
*
*     Get a currently unused FORTRAN logical unit
*
      LUN = 0
      IRC = 0
      DO 10 I=1,KMXLUN
      IF((MCDLUN(I).NE.-1).AND.(MCDLUA(I).EQ.-1)) THEN
         MCDLUA(I) = MCDLUN(I)
         LUN       = MCDLUN(I)
         GOTO 20
      ENDIF
10    CONTINUE
      IRC = -1
20    CONTINUE
#if defined(CERNLIB_VAXVMS)
      IF(LUN.EQ.0) THEN
      CALL LIB$GET_LUN(LUN)
      IF(LUN.EQ.-1) THEN
         IRC = LUN
      ELSE
         IRC = 0
      ENDIF
      ENDIF
#endif
      END
