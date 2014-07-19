*
* $Id: dbildu.F,v 1.1.1.1 1996/02/28 16:24:57 mclareni Exp $
*
* $Log: dbildu.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:57  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBILDU (LUNI, TOPNM, CHOPT)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBILDU (LUNI, TOPNM, CHOPT)                             *
*                                                                      *
*   Initializes the List of Directories to be Updated from the         *
*   journal file.                                                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNI     Logical unit number from which the list is read off     *
*     TOPNM    Name of the Top Directory                               *
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
*               =231 : Illegal top directory name                      *
*               =232 : Illegal logical unit number                     *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       CHOPT*(*), TOPNM*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDILDU (LUNI, TOPNM, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBILDU
  999 END
