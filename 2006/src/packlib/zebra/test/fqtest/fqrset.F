*
* $Id: fqrset.F,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqrset.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQRSET (MODE,LUNP)

C-    MODE = 0 :  break connection for input
C-         -ve :  break connection for output
C-         +ve :  reset DAT forward reference record to null
C-                careful : may only be called just after FZODAT

#include "zebra/mqsys.inc"
#include "zebra/eqlqf.inc"
#include "zebra/fzci.inc"
#include "zebra/fzcx.inc"
C--------------    END CDE                             --------------


      IF (MODE.EQ.0)  THEN
          LUNI = 0
          RETURN
        ENDIF

      IF (MODE.LT.0)  THEN
          LUNX = 0
          RETURN
        ENDIF

      LUNNX = LUNP
      CALL FZLOC (LUNNX,2)
      IF (IFIFOX.NE.2)             RETURN
      IF (IACMOX.GE.3)             RETURN

      IQ(KQSP+LQFX+34) = 0
      IQ(KQSP+LQFX+35) = 7

      CALL FZUDAT (LUNNX,0)
      LUNX = -1
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
