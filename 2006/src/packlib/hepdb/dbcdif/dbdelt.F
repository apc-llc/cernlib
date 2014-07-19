*
* $Id: dbdelt.F,v 1.1.1.1 1996/02/28 16:24:54 mclareni Exp $
*
* $Log: dbdelt.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:54  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBDELT (PATHN, CHOPT)
*     ================================
*
************************************************************************
*                                                                      *
*        SUBR. DBDELT (PATHN, CHOPT)                                   *
*                                                                      *
*   Deletes the directory from the current level downwards as given    *
*   in the path name                                                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Path name to be deleted                                 *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*                                                                      *
*   Called by user,   DBFZUP, DBKEPT, DBACTI                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =171 : Illegal Path name                               *
*               =172 : Cannot find the top directory for the path name *
*               =173 : Error in RZ for reading the dictionary object   *
*               =174 : Error in FZOUT for saving the journal file      *
*               =175 : Error in RZ in writing the dictionary object    *
*               =176 : Error in RZ in purging the dictionary directory *
*               =177 : Error in RZ in deleting the tree                *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATHN*(*), CHOPT*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDDDIR (PATHN, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBDELT
  999 END
