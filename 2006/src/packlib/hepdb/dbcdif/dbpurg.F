*
* $Id: dbpurg.F,v 1.1.1.1 1996/02/28 16:25:01 mclareni Exp $
*
* $Log: dbpurg.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:01  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBPURG (PATHN, KYDAT, KYTIM, CHOPT)
*     ==============================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPURG (PATHN, KYDAT, KYTIM, CHOPT)                     *
*                                                                      *
*   Purges/Deletes objects in a directory PATHN                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KYDAT    Key element number (for option K in CHOPT) or           *
*              Minimum value of Key 1 to be deleted (for option S)     *
*     KYTIM    Cutoff value for the key (for option K) or              *
*              Maximum value of Key 1 to be deleted (for option S)     *
*     CHOPT    Character string with any of the following characters   *
*          A   Deletes all data objects                                *
*          B   Save in the special backup file; not in standard Journal*
*          K   Deletes all data objects for which KEY(KYDAT) .lt. KYTIM*
*          L   Deletes all but the last (one with highest KEY(1) value)*
*              data objects                                            *
*          P   Deletes all data objects with identical start and end   *
*              validity but those having the highest Program Version   *
*              number (i.e., KEY(5) value)                             *
*          S   Deletes all data objects with Serial number (KEY(1)) in *
*              the range KYDAT-KYTIM (the terminal points included)    *
*                                                                      *
*   Called by user,   DBFZUP, DBAUXI                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =111 : Illegal path name                               *
*               =112 : No key or data for the path name                *
*                                                                      *
*     If IQUEST(1) =0, IQUEST(2) carries information on number of      *
*     data objects deleted in the disk                                 *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      CHARACTER       PATHN*(*), CHOPT*(*)
      DIMENSION       IOFF(NSYSDX)
      PARAMETER       (ITIMB=NOF1CK+1, ITIME=NOF2CK+2)
      DATA            IOFF / IDHKSN, IDHPTR, ITIMB,  ITIME, IDHUSI,
     +                       IDHFLG, IDHINS/
*
*     ------------------------------------------------------------------
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
*
      IF (IOPKDX.NE.0) THEN
        IF (KYDAT.GT.NSYSDX) THEN
          KYDTI = KYDAT + NOF2CK + 2 - NSYSDX
        ELSE
          KYDTI = IOFF(KYDAT)
        ENDIF
      ELSE
        KYDTI = KYDAT
      ENDIF
*
      CALL CDPURG (PATHN, KYDTI, KYTIM, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBPURG
  999 END
