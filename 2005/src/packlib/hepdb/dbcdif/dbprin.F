*
* $Id: dbprin.F,v 1.1.1.1 1996/02/28 16:25:01 mclareni Exp $
*
* $Log: dbprin.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:01  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBPRIN (PATHN, CHOPT)
*     ================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPRIN (PATHN, CHOPT)                                   *
*                                                                      *
*   Prints all objects attached to the directory PATHN                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     CHOPT    Character string with any of the following characters   *
*          I   Print the insertion time as well                        *
*          K   Print only the keys                                     *
*          D   Print all data as well as the keys                      *
*                                                                      *
*   Called by user,   DBAUXI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =101 : Illegal path name                               *
*               =102 : No key or data for the path name                *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATHN*(*), CHOPT*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDPRIN (PATHN, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBPRIN
  999 END
