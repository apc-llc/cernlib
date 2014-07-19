*
* $Id: cdehlp.F,v 1.1.1.1 1996/02/28 16:24:09 mclareni Exp $
*
* $Log: cdehlp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:09  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDEHLP (PATHN, LUN, IRC)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDEHLP (PATHN, LUN, IRC*)                               *
*                                                                      *
*   Encode the help information from an ASCII disk file specified by   *
*   the logical unit number LUN with subroutine CDLINC and enter it    *
*   into the data base.                                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Path name of the help subdirectory                      *
*     LUN      Logical unit number of the file where ASCII help        *
*              information reside                                      *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDHELP                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =146 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*), PATHY*80
*
*     ------------------------------------------------------------------
*
* *** Load the top directory information
*
      CALL CDOPTS (' ', IRC)
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)  GO TO 999
      PATHY  = PAT1CT
*
* *** Find the unique directory identifier from the pathname
*
      CALL CDGPID (PATHY, IDN)
      IF (IDN.LE.0) THEN
        IRC    = 146
#if defined(CERNLIB__DEBUG)
        NCHAR  = LENOCC (PATHY)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEHLP : Illegal'//
     +  ' pathname '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Read the data part
*
      CALL CDATOI (LUN, LSTRCL(1), IRC)
      IF (IRC.NE.0)  GO TO 10
*
* *** Now save the bank into the database and drop the bank
*
      CALL CDSNAM (1, IDN, LSTRCL(1), 0, IRC)
*
   10 IF (LSTRCL(1).NE.0) THEN
        CALL MZDROP (IDIVCD, LSTRCL(1), 'L')
        LSTRCL(1) = 0
      ENDIF
*                                                             END CDEHLP
  999 END
