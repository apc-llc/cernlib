*
* $Id: dbrtfz.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbrtfz.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBRTFZ (PATHI, LUNFZ, K1MIN, K1MAX, CHOPT)
*     =====================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBRTFZ (PATHI, LUNFZ, K1MIN, K1MAX, CHOPT)              *
*                                                                      *
*   Copies a part of the directory (in the range K1MIN-K1MAX) into a   *
*   sequential file in the format of the journal file                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHI    Character string describing the input  pathname         *
*     LUNFZ    Logical unit number of the output FZ file               *
*     K1MIN    Minumum serial number (Key 1 value) to be copied        *
*     K1MAX    Maximum serial number (Key 1 value) to be copied        *
*     CHOPT    Character string with any of the following characters   *
*          F   Updates with a fully matched data object (in user keys) *
*                                                                      *
*   Called by user,   DBAUXI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 71 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATHI*(*), CHOPT*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDRTFZ (PATHI, LUNFZ, K1MIN, K1MAX, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBRTFZ
  999 END
