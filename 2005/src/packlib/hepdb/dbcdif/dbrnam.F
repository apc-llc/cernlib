*
* $Id: dbrnam.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbrnam.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBRNAM (PATH, NWMAX, CHTAG)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBRNAM (PATH, *NWMAX*, CHTAG*)                          *
*                                                                      *
*   Retrieves the names of the data elements of a given directory      *
*   from the data base                                                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Character string describing the pathname                *
*     NWMAX    Maximum number of data word elements for CHTAG          *
*              (on return it contains the number of CHTAG filled in)   *
*     CHTAG    Name of the data elements                               *
*                                                                      *
*   Called by user,   DBAUXI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =182 : Illegal path name                               *
*               =201 : DICTIONARY directory not found                  *
*               =202 : No description of data elements for the given   *
*                      path name exists in the data base               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER*(*)   CHTAG(*), PATH
*     ------------------------------------------------------------------
*
      CALL CDRNAM (PATHN, NWMAX, CHTAG, IRC)
      IQUEST(1) = IRC
*                                                             END DBRNAM
  999 END
