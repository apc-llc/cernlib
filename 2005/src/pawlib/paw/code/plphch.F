*
* $Id: plphch.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: plphch.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/15 05/03/94  19.31.04  by  Fons Rademakers
*-- Author :    Marthe Brun
      LOGICAL FUNCTION PLPHCH(KCHAR)
*
#include "paw/pachar.inc"
      CHARACTER*1 KCHAR
      ICODE=ICHAR(KCHAR)
      IF((ICODE.GE.JAMAX.AND.ICODE.LE.JZMAX).OR.
     +   (ICODE.GE.JAMIN.AND.ICODE.LE.JZMIN))THEN
         PLPHCH=.TRUE.
      ELSE
         PLPHCH=.FALSE.
      ENDIF
      END
