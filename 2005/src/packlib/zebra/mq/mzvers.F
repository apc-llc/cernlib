*
* $Id: mzvers.F,v 1.5 1999/06/18 13:30:20 couet Exp $
*
* $Log: mzvers.F,v $
* Revision 1.5  1999/06/18 13:30:20  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.4  1999/06/18 12:04:45  couet
* - The version is now defined in one central place
*
* Revision 1.3  1999/06/14 13:59:23  couet
* - Mods for Y2K in the date/time output
*
* Revision 1.2  1996/04/18 16:12:07  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:17  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE MZVERS

C-    Print Zebra / Kernlib version on IQLOG,  user called

#include "zebra/zheadp.inc"
#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mqsysh.inc"
#include "zebra/q_locf.inc"

#include "zebra/qversion.inc"

      IF (NQLOGD.LT.-1)            GO TO 79

      WRITE (IQLOG,9002) QVERSN,IQDATE,IQTIME
 9002 FORMAT (1X/' ZEBRA SYSTEM',F8.4,' executing, Date/Time',
     +I7.6,1H/,I4)

      WRITE (IQLOG,9003) QVERSN
 9003 FORMAT (1X/1X,'ZEMQ     ',F4.2,' ')

      LQASTO = LOCF (LQ(1)) - 1
      WRITE (IQLOG,9004) LQASTO,LQASTO
 9004 FORMAT (1X/10X,'ZEBRA store base  LQ(0) in /ZEBQ/ at adr',I12,
#if !defined(CERNLIB_HEX)
     F1X,O11,' OCT')
#endif
#if defined(CERNLIB_HEX)
     F1X,Z11,' HEX')
#endif
      CALL KERNGT (IQLOG)
   79 IQUEST(1) = QVERSN * 10000.
      RETURN
      END
