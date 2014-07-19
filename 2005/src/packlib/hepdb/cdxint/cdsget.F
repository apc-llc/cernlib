*
* $Id: cdsget.F,v 1.1.1.1 1996/02/28 16:24:52 mclareni Exp $
*
* $Log: cdsget.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSGET (KYSER, IUDIV, LADD, IRC)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSGET (KYSER, IUDIV, LADD*, IRC*)                      *
*                                                                      *
*   Retrievs one data object from the serial number                    *
*   (Assumes the current directory is set and on successful return     *
*    KEYVCK contains the key vector)                                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KYSER    Serial number of the object to be retrieved             *
*     IUDIV    Division where the object would reside in memory        *
*     LADD     Address of the object                                   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDAIRD, CDDISD                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =101 : Illegal path name of partition                  *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (JBIAS=2)
      DIMENSION       LADD(9), ITIME(MXPACD)
      CHARACTER       PATHY*80
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Iniatilize
*
      IF (LADD(1).NE.0) THEN
        CALL MZDROP (IDIVCD, LADD(1), 'L')
        LADD(1) = 0
      ENDIF
      IRC    = 0
      IF (NKEYCK.EQ.0)                  GO TO 999
      CALL RZCDIR (PATHY, 'R')
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ISTP   = NWKYCK + 1
*
* *** Read in the data
*
      DO 5 I = 1, NPARCD
        ITIME(I) = 1
    5 CONTINUE
      IF (IOPTP.EQ.0) THEN
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        KPNT   = IUHUNT (KYSER, IQ(IPNT+IDHKSN), ISTP*NKEYCK, ISTP)
        IF (KPNT.GT.0) THEN
          KEYVCK(IDHKSN) = (KPNT - IDHKSN) / ISTP + 1
        ELSE
          KEYVCK(IDHKSN) = NKEYCK
        ENDIF
      ELSE
        NKEYS  = NKEYCK
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        NCHR   = LENOCC (PATHY)
        DO 10 JK = 1, NKEYS
          IK     = NKEYS + 1 - JK
          KPNT   = IUHUNT (IK, IQ(IPNT+MPSRCD), NKEYS*ISTP, ISTP)
          IF (KPNT.GT.0) THEN
            KPNT   = IPNT + KPNT - MPSRCD
          ELSE
            KPNT   = IPNT + (IK - 1) * ISTP
          ENDIF
          IF (IQ(KPNT+MOBJCD).GT.KYSER) GO TO 10
          CALL CDPATH (TOP2CT, IK)
          PAT2CT = PATHY(1:NCHR)//'/'//TOP2CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 101
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDSGET : '//
     +         'Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NKEYCK = IQUEST(7)
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          KPNT   = IUHUNT (KYSER, IQ(IPNT+IDHKSN), ISTP*NKEYCK, ISTP)
          IF (KPNT.GT.0) THEN
            KEYVCK(IDHKSN) = (KPNT - IDHKSN) / ISTP + 1
          ELSE
            KEYVCK(IDHKSN) = NKEYCK
          ENDIF
          GO TO 15
   10   CONTINUE
      ENDIF
   15 IOLD1  = IOKYCA(IDHKSN)
      IOLDK  = IOPKCA
      IOKYCA(IDHKSN) = 1
      IOPKCA = 0
      CALL CDKXIN (ITIME, IUDIV, LADD(1), LADD(1), JBIAS, NWKYCK,
     +             KEYVCK, IPREC, IRC)
      IOKYCA(IDHKSN) = IOLD1
      IOPKCA = IOLDK
      IF (IOPTP.NE.0) THEN
        CALL RZCDIR (PATHY, ' ')
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        NKEYCK = IQUEST(7)
      ENDIF
*                                                             END CDSGET
  999 END
