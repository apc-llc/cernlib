*
* $Id: dbrhlp.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbrhlp.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBRHLP (PATH, LUN)
*     =============================
*
************************************************************************
*                                                                      *
*        SUBR. DBRHLP (PATH, LUN)                                      *
*                                                                      *
*   Retrieve the help information from data base and translate it      *
*   with Subroutine DBLIND and also write the ASCII format on a        *
*   given file specified by logical unit number LUN.                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Character string describing the pathname                *
*     LUN      Logical unit number of the file with ASCII data         *
*                                                                      *
*   Called by user,   DBACPL, DBAUXI                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 66 : Illegal logical unit number                     *
*               =182 : The pathname specified does not exist           *
*               =203 : No help directory inside the data base          *
*               =204 : No help information for this path stored yet    *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATH*(*)
*     ------------------------------------------------------------------
*
      CALL CDRHLP (PATH, LUN, IRC)
      IQUEST(1) = IRC
*                                                             END DBRHLP
  999 END
