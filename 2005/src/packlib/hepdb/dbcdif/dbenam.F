*
* $Id: dbenam.F,v 1.1.1.1 1996/02/28 16:24:55 mclareni Exp $
*
* $Log: dbenam.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:55  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBENAM (PATH, NW, CHTAG)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. DBENAM (PATH, NW, CHTAG)                                *
*                                                                      *
*   Enters the name of the data elements of a given directory inside   *
*   the data base. The stored information goes in the DICTIONARY       *
*   directory with KEY(1) set to the unique identifier of this         *
*   directory.                                                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Character string describing the path name               *
*     NW       Number of data elements                                 *
*     CHTAG    Name of each data element (assumed to be 8 character)   *
*                                                                      *
*   Called by user,   DBAUXI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =182 : Illegal path name                               *
*               =183 : Illegal number of data words                    *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER*(*)   CHTAG(*), PATH
*     ------------------------------------------------------------------
*
      CALL CDENAM (PATH, NW, CHTAG, IRC)
      IQUEST(1) = IRC
*                                                             END DBENAM
  999 END
