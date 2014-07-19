*
* $Id: dbkept.F,v 1.1.1.1 1996/02/28 16:24:58 mclareni Exp $
*
* $Log: dbkept.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:58  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBKEPT (PATHS, NPATH, CHOPT)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBKEPT (PATHS, NPATH, CHOPT)                            *
*                                                                      *
*   Deletes all directory trees from the data base except the ones     *
*   specified by the user                                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHS    Path names to be kept                                   *
*     NPATH    Number of paths to be kept                              *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*                                                                      *
*   Called by user,   DBAUXI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =211 : Illegal number of paths                         *
*               =212 : Illegal path name                               *
*               =213 : Conflicting top directory names                 *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER*(*)   PATHS(*), CHOPT
*
*     ------------------------------------------------------------------
*
      CALL CDKEEP (PATHS, NPATH, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBKEPT
  999 END
