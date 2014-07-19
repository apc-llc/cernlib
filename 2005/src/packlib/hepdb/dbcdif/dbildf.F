*
* $Id: dbildf.F,v 1.1.1.1 1996/02/28 16:24:57 mclareni Exp $
*
* $Log: dbildf.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:57  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBILDF (LUNI, CHOPT)
*     ===============================
*
************************************************************************
*                                                                      *
*        SUBR. DBILDF (LUNI, CHOPT)                                    *
*                                                                      *
*   Initializes the List of Directories to be always updated from the  *
*   journal file (ignore the check of the key 1).                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNI     Logical unit number from which the list is read off     *
*     CHOPT    Character string with any of the following characters   *
*          A   Add to the previous information                         *
*          I   Ignore all previous information (also current file)     *
*     Default  Overwrite previous information with the current list    *
*                                                                      *
*   Called by user,   DBACTI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =232 : Illegal logical unit number                     *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       CHOPT*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDILDF (LUNI, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBILDF
  999 END
