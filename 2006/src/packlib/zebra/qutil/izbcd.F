*
* $Id: izbcd.F,v 1.4 1999/06/18 13:31:18 couet Exp $
*
* $Log: izbcd.F,v $
* Revision 1.4  1999/06/18 13:31:18  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.3  1998/09/25 09:33:20  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.2  1996/09/23 12:10:20  cernlib
* add linux
*
* Revision 1.1.1.1  1996/03/06 10:47:14  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
#if defined(CERNLIB_Z32)
#include "qutyz32/izbcd.F"
#elif (defined(CERNLIB_VAXVMS))||(defined(CERNLIB_QMVMI)) \
    ||(defined(CERNLIB_QMDOS))||(defined(CERNLIB_TMO)) \
    ||((defined(CERNLIB_QMLNX) && !defined(CERNLIB_PPC)))
#include "qutyinv/izbcd.F"
#elif (defined(CERNLIB_CONVEX))&&(defined(CERNLIB_SINGLE))
#include "not_cern/qutycvx/izbcd.F"
#elif defined(CERNLIB_CRAY)
#include "not_cern/qutycry/izbcd.F"
#elif defined(CERNLIB_CDC)
#include "not_cern/qutycdc/izbcd.F"
#elif defined(CERNLIB_PDP)
#include "not_cern/qutypdp/izbcd.F"
#else
      FUNCTION IZBCD (CHAR)

C-    This is a very slow stop-gap solution only  !!!

#include "zebra/zbcd.inc"
*
      INTEGER      CHAR(9)


      IZBCD = IUCOMP (CHAR(1),IQLETT(1),63)
      IF (IZBCD.EQ.0)  IZBCD=57
      RETURN
      END
#endif
