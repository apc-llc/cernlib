*
* $Id: kaxrclib.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxrclib.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMXA))
      INTEGER FUNCTION KAXRCL(LUN)
#include "kapack/kax000.inc"
#include "kapack/kax030.inc"
      IF (LBKS(LUN).EQ.0) OPEN (LUN)
      INQUIRE(LUN,RECL=ILEN)
      IF (LBKS(LUN).EQ.0) CLOSE (LUN)
      KAXRCL = ILEN
      END
#endif
