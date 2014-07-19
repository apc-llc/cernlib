*
* $Id: cduvd3.F,v 1.1.1.1 1996/02/28 16:23:53 mclareni Exp $
*
* $Log: cduvd3.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUVD3
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDUVD3                                                  *
*                                                                      *
*   Stores objects in the UVD3 subdirectory using CDSTOR (with update) *
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
      DO 10 K = 1, NOBJCU
        CALL CDSTOR ('//DBL3/DBMU/MCALB/UVD3', LUSECU(K), LKUVCU,
     +               IDIVCU, KEYSCU(1,K), 'DP', IRC)
   10 CONTINUE
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
 1001 FORMAT (/,' CDUVD3 : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDUVD3 : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDUVD3
      END
