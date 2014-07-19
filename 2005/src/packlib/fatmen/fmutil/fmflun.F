*
* $Id: fmflun.F,v 1.1.1.1 1996/03/07 15:18:17 mclareni Exp $
*
* $Log: fmflun.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:17  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFLUN(LUN,IRC)
*CMZ :          04/02/91  12.20.37  by  Jamie Shiers
*-- Author :    Jamie Shiers   04/02/91
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatlun.inc"
      IRC = 0
      DO 10 I=1,KMXLUN
      IF(MFMLUN(I).EQ.LUN) THEN
         MFMLUA(I) = -1
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
