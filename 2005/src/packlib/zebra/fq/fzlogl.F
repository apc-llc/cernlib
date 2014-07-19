*
* $Id: fzlogl.F,v 1.2 1999/06/18 13:29:24 couet Exp $
*
* $Log: fzlogl.F,v $
* Revision 1.2  1999/06/18 13:29:24  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.1.1.1  1996/03/06 10:47:13  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE FZLOGL (LUNPAR,LOGLP)

C-    SET DIAGNOSTIC LOGGING LEVEL

#include "zebra/zunit.inc"
#include "zebra/mqsys.inc"
#include "zebra/eqlqf.inc"
#include "zebra/fzcf.inc"
#include "zebra/fzci.inc"
#include "zebra/fzcx.inc"
*
      DIMENSION    LUNPAR(9), LOGLP(9)


      LUN  = LUNPAR(1)
      LOGL = LOGLP(1)
#if !defined(CERNLIB_QDEVZE)
      LOGL = MIN (LOGL,4)
#endif
      CALL FZLOC (LUN,0)
      IF (LQFF.EQ.0)         RETURN

#if defined(CERNLIB_QPRINT)
      IF (LOGL.GE.0)  WRITE (IQLOG,9022) LUN,LOGL
 9022 FORMAT (1X/' FZLOGL.  File at LUN=',I3,
     F', Diagnostic log level=',I2)
#endif

      IQ(KQSP+LQFF+4) = LOGL
      IF (LUNF.EQ.LUNI)  LOGLVI=LOGL
      IF (LUNF.EQ.LUNX)  LOGLVX=LOGL
      RETURN
      END
