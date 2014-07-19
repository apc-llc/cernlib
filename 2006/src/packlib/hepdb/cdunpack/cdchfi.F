*
* $Id: cdchfi.F,v 1.1.1.1 1996/02/28 16:24:35 mclareni Exp $
*
* $Log: cdchfi.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCHFI (I, CH)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDCHFI (I, CH*)                                         *
*                                                                      *
*   Converts an integer I to an output single character CH (of type    *
*   Character) via the look-up table stored in CSTRCA                  *
*   (Adapted from CHTOI from CERN Library)                             *
*                                                                      *
*   Called by CDLIND                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
      CHARACTER       CH*1
*     ------------------------------------------------------------------
*
      CH = CSTRCA(63:63)
      IF (I.LT.32.OR.I.GT.127) GO TO 999
      CH = CSTRCA(I:I)
*                                                             END CDCHFI
  999 END
