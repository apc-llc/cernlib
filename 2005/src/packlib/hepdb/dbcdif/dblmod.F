*
* $Id: dblmod.F,v 1.1.1.1 1996/02/28 16:25:00 mclareni Exp $
*
* $Log: dblmod.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:00  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBLMOD (PATHN, IDATE, ITIME)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBLMOD (PATHN, IDATE*, ITIME*)                          *
*                                                                      *
*   Finds the date of last modification in a given directory           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IDATE(*) Date (YYMMDD) of last modification                      *
*     ITIME(*) Time (HHMM) for the last modification                   *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =131 : Illegal pathname                                *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATHN*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDLMOD (PATHN, IDATE, ITIME, 'R', IRC)
      IQUEST(1) = IRC
*                                                             END DBLMOD
  999 END
