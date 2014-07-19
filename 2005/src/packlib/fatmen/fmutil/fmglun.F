*
* $Id: fmglun.F,v 1.1.1.1 1996/03/07 15:18:17 mclareni Exp $
*
* $Log: fmglun.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:17  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGLUN(LUN,IRC)
*CMZ :          04/02/91  12.20.37  by  Jamie Shiers
*-- Author :    Jamie Shiers   04/02/91
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatlun.inc"
      LUN = 0
      IRC = 0
      DO 10 I=1,KMXLUN
         IF((MFMLUN(I).NE.-1).AND.(MFMLUA(I).EQ.-1)) THEN
            MFMLUA(I) = MFMLUN(I)
            LUN = MFMLUN(I)
            GOTO 20
         ENDIF
   10 CONTINUE
      IRC = -1
   20 CONTINUE
#if defined(CERNLIB_VAXVMS)
      IF(LUN.EQ.0) THEN
         ISTAT = LIB$GET_LUN(LUN)
#include "fatmen/fatvaxrc.inc"
         IF(LUN.EQ.-1) THEN
            IRC = LUN
         ELSE
            IRC = 0
         ENDIF
      ENDIF
#endif
      END
