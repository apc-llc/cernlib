*
* $Id: cdbkin.F,v 1.1.1.1 1996/02/28 16:23:53 mclareni Exp $
*
* $Log: cdbkin.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDBKIN (PATHN, KEY1S, IUDIV, LBD, LSUP, JBIAS, IRC)
*     ==============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDBKIN (PATHN, KEY1S, IUDIV, LBD*, LSUP, JBIAS, IRC*)   *
*                                                                      *
*   Fetches from disk to a ZEBRA bank data valid for a given Key       *
*   serial number (as stored inside the directory)                     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEY1S    Serial number of the data to be fetched                 *
*     IUDIV    Division index where bank is expected                   *
*     LBD(*)   Address of the bank in memory                           *
*     LSUP     Supporting link of the bank (see MZBOOK)                *
*     JBIAS    Link bias for creating the data bank (see MZBOOK)       *
*     IRC(*)   Return Code                                             *
*               0 : No error                                           *
*              31 : Illegal path name                                  *
*              32 : No key or data in the path name                    *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
************************************************************************
*
#include "caopts.inc"
#include "cdcblk.inc"
#include "ckkeys.inc"
#include "ctpath.inc"
      DIMENSION       LSUP(9), LBD(9), ITIME(MXPACD)
      CHARACTER       PATH*80, PATHN*(*)
*
*     ------------------------------------------------------------------
*
* *** Set the character option
*
      CALL CDOPTS (' ', IRC)
      IF (IRC.NE.0)       GO TO 900
      IOKYCA(IDHKSN) = 1
*
* *** Set the current directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)       GO TO 900
      DO 10 I = 1, NPARCD
        ITIME(I) = 1
   10 CONTINUE
      CALL CDKEYT
*
      IF (NKEYCK.LE.0)  THEN
        IRC    = 32
#if defined(CERNLIB__DEBUG)
        NCHAR  = LENOCC (PAT1CT)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDBKIN : No key '//
     +  'or data for Path Name '//PAT1CT(1:NCHAR)//''')', IARGCD, 0)
#endif
        GO TO 900
      ENDIF
*
* *** Read in the data
*
      CALL VZERO (KEYVCK, NWKYCK)
      KEYVCK(IDHKSN) = KEY1S
      CALL CDKXIN (ITIME, IUDIV, LBD(1), LSUP(1), JBIAS, NWKEY, KEYVCK,
     +             IPREC, IRC)
  900 IOKYCA(IDHKSN) = 0
*                                                             END CDBKIN
  999 END
