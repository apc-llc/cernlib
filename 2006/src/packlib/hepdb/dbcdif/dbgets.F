*
* $Id: dbgets.F,v 1.1.1.1 1996/02/28 16:24:57 mclareni Exp $
*
* $Log: dbgets.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:57  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBGETS (PATHN, LBK, KEYS, CHOPT)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. DBGETS (PATHN, LBK*, KEYS, CHOPT)                       *
*                                                                      *
*   Prepares the database data structure in memory for any required    *
*   Pathname and serial number(s), unless already done.  Returns the   *
*   addresses in memory for the corresponding Key bank(s) and loads    *
*   the data part at link 1 of the Key bank(s).                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the path name               *
*     LBK(*)   Address(es) of Keys bank(s) KYCD. The data bank address *
*              can be obtained as LQ(LBK-1). For several selected      *
*              objects, the Keys banks appear in a linear structure    *
*     KEYS     Vector of keys. KEYS(1) and (2) should contain the      *
*              two limits on the serial number                         *
*     CHOPT    Character string with any of the following characters   *
*          K   read only the keys (no data is required)                *
*          5   specific Program version number required                *
*          6   select objects with insertion time > KEYS(6)            *
*          7   select objects with insertion time < KEYS(7)            *
*          n   consider user key n (where 7 < n < 29 )                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =  2 : Illegal path name                               *
*               = 32 : No keys/data in this directory                  *
*                                                                      *
*     If IQUEST(1) =0, IQUEST(2) carries information on number of      *
*     data objects selected by DBGETS                                  *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      DIMENSION       LBK(9), KEYS(9), KYSR(2)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOP*16
*
*     ------------------------------------------------------------------
*
* *** Reformat character options
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      IOPRDX = 1
      CALL DBOPTM (CHOP)
*
      CALL DBCKEY (KEYS, KEYSDX, MXKYDX)
*
      KYINM   = KEYS(6)
      KYSR(1) = KEYS(1)
      KYSR(2) = KEYS(2)
      CALL CDGETDB (PATHN, LBK, MASKDX, KEYSDX, KYSR, KYINM, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBGETS
  999 END
