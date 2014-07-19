*
* $Id: dblkey.F,v 1.1.1.1 1996/02/28 16:25:00 mclareni Exp $
*
* $Log: dblkey.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:00  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBLKEY (PATHN, KEY1, IDATE, ITIME)
*     =============================================
*
************************************************************************
*                                                                      *
*        SUBR. DBLKEY (PATHN, KEY1*, IDATE*, ITIME*)                   *
*                                                                      *
*   Finds the latest inserted object for a given directory             *
*   (Adapted from P.Bagnaia)                                           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEY1     The highest key(1) of the directory                     *
*     IDATE    Date (YYMMDD) of the insertion of the last element      *
*     ITIME    Time (HHMM)   "   "      "      "  "    "     "         *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =131 : Illegal pathname                                *
*               =132 : Illegal number of keys in the directory         *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      CHARACTER       PATHN*(*)
      DIMENSION       KEY(MXDMCK)
*
*     ------------------------------------------------------------------
*
      CALL CDLKEY (PATHN, KEYSDX, IDATE, ITIME, 'K', IRC)
      IQUEST(1) = IRC
      KEY1      = KEYSDX(IDHKSN)
*                                                             END DBLKEY
  999 END
