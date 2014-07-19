*
* $Id: dbsrtm.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbsrtm.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBSRTM (PATHN, MNDAT, MNTIM, MXDAT, MXTIM, IUDIV, LSUP,
     +                   IPREC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBSRTM (PATHN, MNDAT, MNTIM, MXDAT, MXTIM, IUDIV, LSUP*,*
*                      IPREC*)                                         *
*                                                                      *
*   Fetches from disk to memory an object inserted between certain     *
*   time                                                               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     MNDAT    Minimum date for insertion                              *
*     MNTIM    Minimum time for insertion                              *
*     MXDAT    Maximum date for insertion                              *
*     MXTIM    Maximum time for insertion                              *
*     IUDIV    Division index where bank is expected                   *
*     LSUP     Address of the bank in memory                           *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points; if*
*              IPREC < 0, data are stored with IPREC insignificant     *
*              digits left to the decimal point.)                      *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 31 : Illegal path name                               *
*               = 32 : No keys/data in this directory                  *
*               = 33 : No valid data for the given range of insertion  *
*                      time                                            *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (JBIAS=2)
      DIMENSION       LSUP(9), ITIME(MXPACD)
      CHARACTER       PATHN*(*), PATHY*80
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Initialise the option array
*
      CALL CDOPTS (' ', IRC)
      IF (IRC.NE.0)       GO TO 900
      IOKYCA(IDHKSN) = 1
*
* *** Set the current directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)       GO TO 900
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
      DO 10 I = 1, NPARCD
        ITIME(I) = 1
   10 CONTINUE
      CALL CDKEYT
*
* *** Check the number of keys
*
      IF (NKEYCK.LE.0)  THEN
        IRC    = 32
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' DBSRTM : No key '//
     +  'or data for Path Name '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 900
      ENDIF
*
* *** Check the validity limits from the Keys
*
      MRET   = 0
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ISTP   = NWKYCK + 1
*
      IF (IOPTP.EQ.0)  THEN
*
*  **   For non-partitioned dataset
*
        DO 20 IK = 1, NKEYCK
*
          IPNT   = KOFSCD + LCDRCD + IKDRCD + (IK-1) * ISTP
          CALL CDUPTM (IDATE, ITIMX, IQ(IPNT+IDHINS), IRC)
          IF ((IDATE.GT.MNDAT.AND.IDATE.LT.MXDAT) .OR.
     +        (IDATE.EQ.MNDAT.AND.ITIMX.GE.MNTIM.AND.MNDAT.NE.MXDAT).OR.
     +        (IDATE.EQ.MXDAT.AND.ITIMX.LE.MXTIM.AND.MNDAT.NE.MXDAT).OR.
     +        (IDATE.EQ.MNDAT.AND.ITIMX.GE.MNTIM.AND.ITIMX.LE.MXTIM.AND.
     +         MNDAT.EQ.MXDAT))  THEN
*
            IF (MRET.NE.0.AND.MRET.LT.IQ(IPNT+IDHINS))  THEN
              IF (LSUP(1).NE.0)  THEN
                CALL MZDROP (IUDIV, LSUP, ' ')
                LSUP(1) = 0
              ENDIF
              CALL VZERO (KEYVCK, NWKYCK)
              KEYVCK(IDHKSN) = IK
              CALL CDKXIN (ITIME, IUDIV, LSUP(1), LSUP(1), JBIAS, NWKEY,
     +                     KEYVCK, IPREC, IRC)
*
              IF (IRC.EQ.0)  THEN
                MRET   = KEYVCK(IDHINS)
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.1) THEN
                  CALL UCOPY (KEYVCK(1), IARGCD, 5)
                  IARGCD(6) = IDATE
                  IARGCD(7) = ITIMX
                  CALL CDPRNT (LPRTCD, '(/,'' DBSRTM : Data with Ke'//
     +                 'y'',2I8,2I10,I6,'' retrieved for '//PATHY(1:40)
     +                 //''',/,10X,''Created on the '',I8,'' at '','//
     +                 'I6)', IARGCD, 7)
                ENDIF
#endif
*
              ELSE
                MRET   = 0
                IF (LSUP(1).NE.0) THEN
                  CALL MZDROP (IUDIV, LSUP, ' ')
                  LSUP(1) = 0
                ENDIF
              ENDIF
            ENDIF
*
          ENDIF
*
   20   CONTINUE
*
      ELSE
*
*  **   For partitioned dataset
*
        NKEYS  = NKEYCK
        KST    = NWKYCK + 1
        DO 40 JK = 1, NKEYS
          ICURCT = NKEYS + 1 - JK
*
          CALL RZCDIR (PATHY(1:NCHAR), ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 31
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' DBSRTM : '//
     +         'Illegal Path Name '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
            GO TO 900
          ENDIF
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          KPNT   = IUHUNT (ICURCT, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*KST, KST)
          IF (KPNT.NE.0) THEN
            IPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            IPNT   = KOFSCD + LCDRCD + IKDRCD + (ICURCT - 1) * KST
          ENDIF
          CALL CDUPTM (IDATE, ITIMX, IQ(IPNT+IDHINS), IRC)
          IF ((IDATE.LT.MNDAT).OR.(IDATE.EQ.MNDAT.AND.ITIMX.LT.MNTIM))
     +                        GO TO 40
*
          CALL CDPATH (TOP1CT, ICURCT)
          PATHY = PATHY(1:NCHAR)//'/'//TOP1CT
          CALL RZCDIR (PATHY, ' ')
          IF (IQUEST(1).EQ.0)  THEN
            IRC    = 31
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' DBSRTM : '//
     +         'Illegal Path Name '//PATHY//''')', IARGCD, 0)
#endif
            GO TO 900
          ENDIF
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          ISTP   = NWKYCK + 1
          CALL CDKEYT
          DO 30 IK = 1, NKEYCK
            IPNT   = KOFSCD + LCDRCD + IKDRCD + (IK-1) * ISTP
            CALL CDUPTM (IDATE, ITIMX, IQ(IPNT+IDHINS), IRC)
            IF ((IDATE.GT.MNDAT.AND.IDATE.LT.MXDAT) .OR.
     +          (IDATE.EQ.MNDAT.AND.ITIMX.GE.MNTIM.AND.MNDAT.NE.MXDAT)
     +      .OR.(IDATE.EQ.MXDAT.AND.ITIMX.LE.MXTIM.AND.MNDAT.NE.MXDAT)
     +      .OR.(IDATE.EQ.MNDAT.AND.ITIMX.GE.MNTIM.AND.ITIMX.LE.MXTIM
     +           .AND.MNDAT.EQ.MXDAT))  THEN
*
              IF (MRET.NE.0.AND.MRET.LT.IQ(IPNT+IDHINS))  THEN
                IF (LSUP(1).NE.0)  THEN
                  CALL MZDROP (IUDIV, LSUP, ' ')
                  LSUP(1) = 0
                ENDIF
                CALL VZERO (KEYVCK, NWKYCK)
                KEYVCK(IDHKSN) = IK
                CALL CDKXIN (ITIME, IUDIV, LSUP(1), LSUP(1), JBIAS,
     +                       NWKEY, KEYVCK, IPREC, IRC)
*
                IF (IRC.EQ.0)  THEN
*
                  MRET   = KEYVCK(IDHINS)
#if defined(CERNLIB__DEBUG)
                  IF (IDEBCD.GT.1) THEN
                    CALL UCOPY (KEYVCK(1), IARGCD, 5)
                    IARGCD(6) = IDATE
                    IARGCD(7) = ITIMX
                    CALL CDPRNT (LPRTCD, '(/,'' DBSRTM : Data with Ke'//
     +                  'y'',2I8,2I10,I6,'' retrieved for '//PATHY(1:40)
     +                   //''',/,10X,''Created on the '',I8,'' at '','//
     +                   'I6)', IARGCD, 7)
                  ENDIF
#endif
*
                ELSE
*
                  MRET   = 0
                  IF (LSUP(1).NE.0) THEN
                    CALL MZDROP (IUDIV, LSUP, ' ')
                    LSUP(1) = 0
                  ENDIF
                ENDIF
              ENDIF
*
            ENDIF
*
   30     CONTINUE
*
          IF (MRET.NE.0)      GO TO 900
   40   CONTINUE
*
      ENDIF
*
  900 IOKYCA(IDHKSN) = 0
      IQUEST(1) = IRC
      IF (MRET.EQ.0) THEN
         IQUEST(1) = 33
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          IARGCD(1) = MNDAT
          IARGCD(2) = MNTIM
          IARGCD(3) = MXDAT
          IARGCD(4) = MXTIM
          CALL CDPRNT (LPRTCD, '(/,'' DBSRTM : No data in '//PATHY(1:40)
     +         //' inserted between '',2I8,'' and '',2I8)', IARGCD, 4)
        ENDIF
#endif
      ENDIF
*                                                             END DBSRTM
  999 END
