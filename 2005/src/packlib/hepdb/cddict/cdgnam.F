*
* $Id: cdgnam.F,v 1.1.1.1 1996/02/28 16:24:09 mclareni Exp $
*
* $Log: cdgnam.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:09  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDGNAM (PATHN, CHTAG, IOBJ, IRC)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDGNAM (PATHN, CHTAG, IOBJ*, IRC*)                      *
*                                                                      *
*   Identifies the object element number from its name and the name    *
*   of the directory path name                                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     CHTAG    Name of the data element                                *
*     IOBJ     Object element number (0 if not found)                  *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =146 : Illegal path name                               *
*               =147 : DICTIONARY directory not found                  *
*               =151 : No description of data elements for the given   *
*                      path name exists in the data base               *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*), CHTAG*(*), CTAG*8, PATHY*80
*
*     ------------------------------------------------------------------
*
* *** Load the top directory information
*
      IOBJ   = 0
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)  GO TO 999
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
*
* *** Find the unique directory identifier from the pathname
*
      CALL CDGPID (PATHY, IDN)
      IF (IDN.LE.0) THEN
        IRC    = 146
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGNAM : Illegal'//
     +  ' path name '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Set the current directory to DICTIONARY and retrieve object
*
      PAT2CT = '//'//TOPNCD(1:NCHRCD)//'/DICTIONARY'
      CALL RZCDIR (PAT2CT, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 147
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGNAM : Path na'//
     +  'me '//PAT2CT(1:NCHRCD+13)//' not found'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      CALL CDKEYT
      ISTP   = NWKYCK + 1
      IPNT   = KOFSCD + LCDRCD + IKDRCD
      KPNT   = IUHUNT (IDN, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
      IF (KPNT.LE.0) THEN
        IRC    = 151
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGNAM : No info'//
     +  'rmation for '//PATHY(1:NCHAR)//' inside Data Base'')',IARGCD,0)
#endif
        GO TO 999
      ENDIF
      KEY1S  = (KPNT - IDHKSN) / ISTP + 1
      CALL VZERO (KEYVCK, NWKYCK)
      KEYVCK(1) = KEY1S
      IF (LSTRCL(1).NE.0) THEN
        CALL MZDROP (IDISCD, LSTRCL(1), 'L')
        LSTRCL(1) = 0
      ENDIF
      IOLD1  = IOKYCA(IDHKSN)
      IOLDK  = IOPKCA
      IOPKCA = 0
      IOKYCA(IDHKSN) = 1
      ITIME  = 1
      CALL CDKXIN (ITIME, IDISCD, LSTRCL(1), LSTRCL(1), 2, NWKYCK,
     +             KEYVCK, IPREC, IRC)
      IOPKCA = IOLDK
      IOKYCA(IDHKSN) = IOLD1
      IF (IRC.NE.0) THEN
        CALL MZDROP (IDISCD, LSTRCL(1), 'L')
        GO TO 999
      ENDIF
*
* *** Now store the information in the user array
*
      IPNT   = KOFUCD + LSTRCL(1)
      NWDS   = IQ(IPNT-1) / 2
      DO 10 I = 1, NWDS
        CALL UHTOC (IQ(IPNT+1), 4, CTAG, 8)
        IF (CTAG.EQ.CHTAG) THEN
          IOBJ   = I
          GO TO 20
        ENDIF
        IPNT   = IPNT + 2
   10 CONTINUE
   20 CALL MZDROP (IDISCD, LSTRCL(1), 'L')
      LSTRCL(1) = 0
*                                                             END CDGNAM
  999 END
