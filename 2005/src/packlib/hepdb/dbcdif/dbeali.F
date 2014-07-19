*
* $Id: dbeali.F,v 1.1.1.1 1996/02/28 16:24:55 mclareni Exp $
*
* $Log: dbeali.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:55  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBEALI (PATH, ALIAS, IFLAG)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBEALI (PATH, ALIAS, IFLAG)                             *
*                                                                      *
*   Enter the alias name of a given directory either only for this     *
*   session or permanently in the data base.                           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Character string specifying the directory path name     *
*     ALIAS    Character string specifying the alias name              *
*     IFLAG    Flag (0 if to be entered only for this session;         *
*                    1 if to be entered for this session and D.B.)     *
*                                                                      *
*   Called by user,   DBFZUP, DBAUXI                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =182 : Illegal path name                               *
*               =185 : Illegal top directory name                      *
*               =187 : FZOUT fails to write on the sequential file     *
*               =188 : Error in RZ for writing to the R.A. file        *
*               =201 : Dictionary directory not found                  *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATH*(*), ALIAS*(*)
*     ------------------------------------------------------------------
*
      CALL CDEALI (PATH, ALIAS, IFLAG, IRC)
      IQUEST(1) = IRC
*                                                             END DBEALI
  999 END
