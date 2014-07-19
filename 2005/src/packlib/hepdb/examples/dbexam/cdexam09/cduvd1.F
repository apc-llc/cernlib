*
* $Id: cduvd1.F,v 1.1.1.1 1996/02/28 16:23:52 mclareni Exp $
*
* $Log: cduvd1.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUVD1
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDUVD1                                                  *
*                                                                      *
*   Stores objects in the UVD1 subdirectory using CDSTOR               *
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
        CALL CDSTOR ('//DBL3/DBMU/MCALB/UVD1', LUSECU(K), LKUVCU,
     +               IDIVCU, KEYSCU(1,K), 'DY', IRC)
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
 1001 FORMAT (/,' CDUVD1 : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDUVD1 : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDUVD1
      END
