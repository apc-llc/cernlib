*
* $Id: cduvd4.F,v 1.1.1.1 1996/02/28 16:23:53 mclareni Exp $
*
* $Log: cduvd4.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUVD4
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDUVD4                                                  *
*                                                                      *
*   Stores objects in the UVD4 subdirectory using CDSTOM (with update) *
*                                                                      *
*   Called by CEXAM09                                                  *
*                                                                      *
************************************************************************
*
#include "cstor9.inc"
#include "cuser9.inc"
*     ------------------------------------------------------------------
*
* *** Get Real time at the beginning of the loop
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
*
* *** Now save the objects
*
      CALL CDSTOM ('//DBL3/DBMU/MCALB/UVD4', LUSECU, LKBKCU, IDIVCU,
     +             NKEYCU, NOBJCU, KEYSCU, KEYSCU, 'DP', IRC)
*
* *** Get Real time at the end of the loop
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
 1001 FORMAT (/,' CDUVD4 : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDUVD4 : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDUVD4
      END
