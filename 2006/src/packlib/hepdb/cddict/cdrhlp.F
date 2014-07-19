*
* $Id: cdrhlp.F,v 1.1.1.1 1996/02/28 16:24:10 mclareni Exp $
*
* $Log: cdrhlp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:10  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRHLP (PATHN, LUN, CHOPT, IRC)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRHLP (PATH, LUN, IRC*)                                *
*                                                                      *
*   Retrieve the help information from data base and translate it      *
*   with Subroutine CDLIND and also write the ASCII format on a        *
*   given file specified by logical unit number LUN.                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Character string describing the pathname                *
*     LUN      Logical unit number of the file with ASCII data         *
*     CHOPT   	Character options                                       *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDHELP                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 66 : Illegal logical unit number                     *
*               =146 : The pathname specified does not exist           *
*               =155 : No help directory inside the data base          *
*               =156 : No help information for this path stored yet    *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*), PATHY*80
      CHARACTER*(*)   CHOPT
*
*     ------------------------------------------------------------------
*
* *** Load the top directory information
*
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
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRHLP : Illegal'//
     +  ' pathname '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
      IF (LUN.LE.0) THEN
        IRC        = 66
        IQUEST(11) = LUN
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRHLP : Illegal'//
     +  ' unit number '',I10,'' for ASCII file'')', IQUEST(11), 1)
#endif
        GO TO 999
      ENDIF
*
* *** Set the current directory to HELP and retrieve object
*
      PAT2CT = '//'//TOPNCD(1:NCHRCD)//'/HELP'
      CALL RZCDIR (PAT2CT, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 155
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRHLP : Path na'//
     +  'me '//PAT2CT(1:NCHRCD+7)//' not found'')', IARGCD, 0)
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
        IRC    = 156
#if defined(CERNLIB__DEBUG)
        NCHAR  = LENOCC (PATHY)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRHLP : No help'//
     +  ' information for '//PATHY(1:NCHAR)//' available'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      KEY1S  = (KPNT - IDHKSN) / ISTP + 1
      CALL VZERO (KEYVCK, NWKYCK)
      KEYVCK(IDHKSN) = KEY1S
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
      CALL CDAFRI (LUN, LSTRCL(1), CHOPT, IRC)
*
      IF (LSTRCL(1).NE.0) THEN
        CALL MZDROP (IDISCD, LSTRCL(1), 'L')
        LSTRCL(1) = 0
      ENDIF
*                                                             END CDRHLP
  999 END
