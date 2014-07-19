*
* $Id: sifam5.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sifam5.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIFAM5
C
C
C       DUMMY ROUTINE FOR  TEST CMD:    PRINT XXX,YYY, .....
C
C
C
#include "sigma/sigc.inc"
C
      KISTRI=0
      IF(KLASS.GT.2) GOTO 9999
      IF(KLASS.EQ.2)KISTRI=1
C
      CALL SIPRIN (KISTRI)
C
      ISTRI=0
C
      RETURN
9999      CALL SINERR(41)
 999  END
