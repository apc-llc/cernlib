*
* $Id: mveuns.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: mveuns.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE MVEUNS (CHPR,ND)

C-    Scan CHPR for unseen character, print message
C-    and replace by !

#include "kerngent/mkcde.inc"

      CHARACTER    CHPR*(*)


      NU = LEN (CHPR)
   24 JUNS = ICLUNS (CHPR,1,NU)
      IF (JUNS.EQ.0)               RETURN

      JCH = ICHAR(CHPR(JUNS:JUNS))
      CHPR(JUNS:JUNS) = '!'
      WRITE (ITB,9024) JUNS+ND,JCH
      GO TO 24

 9024 FORMAT (10X,'Character',I4,' is unseen of value',I4)
      END
