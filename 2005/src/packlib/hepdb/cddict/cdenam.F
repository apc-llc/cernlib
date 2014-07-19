*
* $Id: cdenam.F,v 1.1.1.1 1996/02/28 16:24:09 mclareni Exp $
*
* $Log: cdenam.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:09  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDENAM (PATHN, NW, CHTAG, IRC)
*     =========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDENAM (PATHN, NW, CHTAG, IRC*)                         *
*                                                                      *
*   Enters the name of the data elements of a given directory inside   *
*   the data base. The stored information goes in the DICTIONARY       *
*   directory with KEY(1) set to the unique identifier of this         *
*   directory.                                                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the path name               *
*     NW       Number of data elements                                 *
*     CHTAG    Name of each data element (assumed to be 8 character)   *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDNAME                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =146 : Illegal path name                               *
*               =150 : Illegal number of data words                    *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
      CHARACTER*(*)   CHTAG(*)
      CHARACTER       CTAG*8, PATHN*(*), PATHY*80
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
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENAM : Illegal'//
     +  ' path name '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
      IF (NW.LE.0) THEN
        IRC        = 150
        IQUEST(11) = NW
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENAM : Illegal'//
     +  ' number of data words '',I12)', IQUEST(11), 1)
#endif
        GO TO 999
      ENDIF
*
* *** Book the bank and fill it up with appropriate information
*
      NUM   = 2 * NW
      IF (LSTRCL(1).NE.0) THEN
        CALL MZDROP (IDISCD, LSTRCL(1), 'L')
        LSTRCL(1) = 0
      ENDIF
      CALL CDBANK (IDISCD, LSTRCL(1), LSTRCL(1), 2, 'DNAM', 0, 0, NUM,
     +             5, -1, IRC)
      IF (IRC.NE.0)  GO TO 999
      IPNT = KOFUCD + LSTRCL(1)
      DO 10 I = 1, NW
        CTAG   = CHTAG(I)
        CALL UCTOH (CTAG, IQ(IPNT+1), 4, 8)
        IPNT   = IPNT + 2
   10 CONTINUE
*
* *** Now store the information inside data base
*
      CALL CDSNAM (2, IDN, LSTRCL(1), 0, IRC)
      CALL MZDROP (IDISCD, LSTRCL(1), 'L')
      LSTRCL(1) = 0
*                                                             END CDENAM
  999 END
