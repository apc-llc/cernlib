*
* $Id: cddinf.F,v 1.1.1.1 1996/02/28 16:24:08 mclareni Exp $
*
* $Log: cddinf.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:08  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDINF (IFLG, PATHN, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDINF (PATHN, NW, CHTAG, IRC*)                         *
*                                                                      *
*   Deletes help information or names of the data elements of a given  *
*   directory from the stored information in the data base.            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IFLG     Flag for help information(1) or name of data elements(2)*
*     PATHN    Character string describing the path name               *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDHELP, CDNAME                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =146 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*), PATHY*80
*
*     ------------------------------------------------------------------
*
* *** Load the top directory information
*
      CALL CDOPTS (' ', IRC)
      IOPDCA = 1
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
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDINF : Illegal'//
     +  ' path name '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Now store the information inside data base
*
      CALL CDSNAM (IFLG, IDN, 0, 0, IRC)
*                                                             END CDDINF
  999 END
