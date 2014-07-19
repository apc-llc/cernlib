*
* $Id: cdonfk.F,v 1.1.1.1 1996/02/28 16:23:46 mclareni Exp $
*
* $Log: cdonfk.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDONFK
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDONFK                                                  *
*                                                                      *
*   Tests CDUSEM with option 'S' and for 50 user keys                  *
*                                                                      *
*   Called by CEXAM04                                                  *
*                                                                      *
************************************************************************
*
#include "cstor4.inc"
#include "cuser4.inc"
      PARAMETER       (NOFF2=12)
      DIMENSION       KEYS(100), MASK(100)
      CHARACTER       CHOPT*8
*
*     ------------------------------------------------------------------
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
      CALL VZERO (MASK, 100)
      CALL VZERO (KEYS, 100)
      CALL UCTOH ('CR05', KEYS(NOFF2+8), 4, 4)
      MASK(NOFF2+8) = 1
      CALL CDPKTS (830101, 90000, ITIME, IRC)
      CHOPT = 'SK'
*
* *** -------------- CRATES ---------------------
*
      CALL CDUSEM ('//DBL3/FAKE/CRATES', LKTRCU(1), ITIME, MASK, KEYS,
     +             CHOPT, IRC)
*
* *** Print linear structure
*
      CALL DZSHOW ('LKTRCU  ', IDIVCU, LKTRCU(1), 'LVB', 0, 0, 0, 0)
*
* *** Test subroutine CDPRIN
*
      CALL CDPRIN ('//DBL3/FAKE/CRATES', 'K', IRC)
#if defined(CERNLIB__DEBUG)
      CALL CDPRIN ('//DBL3/FAKE/CRATES', 'D', IRC)
#endif
      CALL CDFREE ('//DBL3/FAKE/CRATES', LKTRCU(1), MASK, KEYS, CHOPT,
     +             IRC)
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
 1001 FORMAT (/,' CDONFK : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDONFK : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDONFK
      END
