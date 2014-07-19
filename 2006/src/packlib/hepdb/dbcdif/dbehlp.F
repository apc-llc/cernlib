*
* $Id: dbehlp.F,v 1.1.1.1 1996/02/28 16:24:55 mclareni Exp $
*
* $Log: dbehlp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:55  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBEHLP (PATH, LUN)
*     =============================
*
************************************************************************
*                                                                      *
*        SUBR. DBEHLP (PATH, LUN)                                      *
*                                                                      *
*   Encode the help information from an ASCII disk file specified by   *
*   the logical unit number LUN with subroutine DBLINC and enter it    *
*   into the data base.                                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Path name of the help subdirectory                      *
*     LUN      Logical unit number of the file where ASCII help        *
*              information reside                                      *
*                                                                      *
*   Called by user,   DBAUXI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 66 : Illegal logical unit number                     *
*               = 67 : File too long; no space in buffer               *
*               =182 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATH*(*)
*     ------------------------------------------------------------------
*
      CALL CDEHLP (PATH, LUN, IRC)
      IQUEST(1) = IRC
*                                                             END DBEHLP
  999 END
