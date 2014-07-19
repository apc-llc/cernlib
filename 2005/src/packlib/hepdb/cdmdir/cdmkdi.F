*
* $Id: cdmkdi.F,v 1.1.1.1 1996/02/28 16:24:16 mclareni Exp $
*
* $Log: cdmkdi.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:16  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDMKDI (PATHN, NWKEY, CHFOR, CHTAG, MXKP, IPREC, DELTA,
     +                   CHOPT, IRC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDMKDI (PATHN, NWKEY, CHFOR, CHTAG, MXKP, IPREC, DELTA, *
*                      CHOPT, IRC*)                                    *
*                                                                      *
*   Creates Directories with the conventions of HEPDB package for      *
*   partitioned and normal data sets                                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Path name of the directory                              *
*     NWKEY    Number of words associated to the keys at the lowest    *
*              level (If some directory in the pathname at a higher    *
*              level does not exist, it is created with 9 keys)        *
*     CHFOR    Character variable describing each element of the key   *
*              vector at the lowest level (at higher level it is set   *
*              to default, i.e., 'IIIIIIIII')                          *
*     CHTAG    Character array defined as CHARACTER*8 (NWKEY)          *
*     MXKP     Maximum number of objects in each partition             *
*     IPREC    Precision word sepcifying the number of significant     *
*              digits to be stored; (If IPREC > 0, data are stored     *
*              with IPREC significant digits right to the decimal      *
*              points; if IPREC < 0, data are stored with IPREC        *
*              insignificant digits left to the decimal point.)        *
*     DELTA    Variable specifying the absolute value below which data *
*              is treated as zero                                      *
*     CHOPT    Character string with any of the following characters   *
*          C   Data in the directory will be compressed by default     *
*          P   Create partitioned subdirectories for the pathname      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDDONT, CDENFZ, CDENTB, CDFZUP, CDMDIR, CDKOUT, CDPART   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 47 : The Directory already exists                    *
*               = 48 : Error in directory search sequence              *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (JBIAS=2)
      CHARACTER*(*)   PATHN, CHFOR, CHOPT, CHTAG(*)
      CHARACTER       PATHL*255
#include "zebra/q_jbit.inc"
* Ignoring t=pass
#include "zebra/q_sbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Assumes the top directory information is already loaded
*
      KEY7   = KEY7CK
      KEY7CK = 0
      PATHL  = ' '
      IRC    = 0
      ICMPF  = IOPCCA
      MXK    = MXKP
      NSYS   = NOF2CK + 2*NPARCD
      IF (MXK.LT.1)    MXK   = MXKPCK
      IF (IOPZCA.EQ.1) ICMPF = 2
*
* *** Check if the directory exists as yet
*
      IENCH  = NCHRCD + 1
      I      = NCHRCD + 2
      IP     = NCHRCD + 3
      ISTCH  = IP
      NCH    = LENOCC (PATHN)
      IPART  = 0
      IF (IP.LE.NCH) THEN
        CALL RZCDIR (PATHN, 'Q')
        IF (IQUEST(1).EQ.0) THEN
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          IF (NKEYCK.GT.0)
     +      IPART  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
          IF (IOKYCA(IDHKSN).EQ.0) THEN
            IF (IPART.EQ.0) THEN
              IRC    = 47
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) THEN
                PAT3CT = PATHN
                CALL CDPRNT (LPRTDB, '(/,'' CDMKDI : Existing directo'//
     +               'ry is in conflict with Path name '',/,9X,'' '//
     +               PAT3CT//' '')', IARGDB, 0)
              ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
            ELSE IF (IOPPCD.EQ.0) THEN
              GO TO 30
#endif
            ENDIF
          ENDIF
          GO TO 999
        ELSE
          IPART  = IOPPCA
        ENDIF
      ENDIF
*
* *** Save the Directory creation information in the FZ file
*
      CALL CDSDIR (PATHN, NWKEY, CHFOR, CHTAG, MXK, IPREC, DELTA,
     +             CHOPT, IRC)
      IF (IOPPCD.NE.0.OR.IRC.NE.0) GO TO 999
#if defined(CERNLIB__P3CHILD)
      GO TO 999
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Create a new (tree of) director(ies) - from the first
* *** non-existing name
* *** Go down to the lowest existing directory in PATHN
*
   10 I      = I + 1
      IF (PATHN(I:I).EQ.'/')  THEN
        CALL RZCDIR (PATHN(1:I-1), 'Q')
        IF (IQUEST(1).NE.0)        GO TO 15
        IENCH  = I -1
      ELSE IF (I.GE.NCH)  THEN
        CALL RZCDIR (PATHN(1:I), 'Q')
        IF (IQUEST(1).NE.0)        GO TO 15
        GO TO 30
      ENDIF
      GO TO 10
*
* *** Lock the directory if needed
*
   15 IF (IOPSCD.NE.0) THEN
        PATHL  = PATHN(1:IENCH)
        CALL RZCDIR (PATHL, ' ')
        CALL RZLOCK ('CDMKDI')
      ENDIF
*
* *** Create new directories
*
      ISTCH  = IENCH + 2
      IF (ISTCH.GT.NCH)            GO TO 90
      TOP1CT = PATHN(ISTCH:ISTCH)
      J      = 0
      ISTCH  = ISTCH + 1
      I      = ISTCH
   20 IF (PATHN(I:I).EQ.'/')  THEN
        CALL RZCDIR (PATHN(1:IENCH), ' ')
*
*  **   Create the intermediate level directory with default values
*
        CALL RZMDIR (TOP1CT, NSYS, CHFOR, CHTAG)
        IF (IQUEST(1).NE.0)        GO TO 90
        IF (PATHL.NE.' ') THEN
          CALL RZCDIR (PATHL, ' ')
          CALL RZFREE ('CDMKDI')
          PATHL  = ' '
          CALL RZCDIR (PATHN(1:IENCH), ' ')
        ENDIF
        CALL CDCDIC (TOP1CT, ICMPF, IPREC, DELTA, IRC)
        IF (IRC.NE.0)              GO TO 95
        IF (IOPSCD.NE.0) THEN
          PATHL  = PATHN(1:IENCH)
          CALL RZCDIR (PATHL, ' ')
          CALL RZLOCK ('CDMKDI')
        ENDIF
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
        IENCH  = I - 1
        I      = I + 1
        TOP1CT = PATHN(I:I)
        J      = 0
*
      ELSE
        J      = J + 1
        TOP1CT = TOP1CT(1:J)//PATHN(I:I)
      ENDIF
      IF (I.LT.NCH)  THEN
        I      = I + 1
        GO TO 20
      ENDIF
*
      IF (IENCH.GE.0) CALL RZCDIR (PATHN(1:IENCH),' ')
      CALL RZMDIR (TOP1CT, NWKEY, CHFOR, CHTAG)
      IF (IQUEST(1).NE.0)          GO TO 90
      IF (PATHL.NE.' ') THEN
        CALL RZCDIR (PATHL, ' ')
        CALL RZFREE ('CDMKDI')
        PATHL  = ' '
        IF (IENCH.GE.0) CALL RZCDIR (PATHN(1:IENCH), ' ')
      ENDIF
      CALL CDCDIC (TOP1CT, ICMPF, IPREC, DELTA, IRC)
      IF (IRC.NE.0)                GO TO 95
      IF (IOPSCD.NE.0) THEN
        PATHL  = PATHN(1:IENCH)
        CALL RZCDIR (PATHL, ' ')
        CALL RZLOCK ('CDMKDI')
      ENDIF
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
      IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
      CALL RZCDIR (PATHN, ' ')
      IF (IQUEST(1).NE.0)          GO TO 90
      IF (IPART.EQ.0)              GO TO 95
*
* *** Now create Partitioned subdirectory
*
   30 NK    = IQUEST(7)
      CALL VZERO (KEYNCK, NSYS)
      IF (NK.GT.0) THEN
*
*  **   Get the keys of the last data inserted
*
        CALL CDPATH (TOP1CT, NK)
        CALL RZCDIR (TOP1CT, ' ')
        IF (IQUEST(1).NE.0)        GO TO 90
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        ISTP   = NWKYCK + 1
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        KEYNCK(MOBJCD) = IQ(IPNT+IDHKSN)
        IF (NKEYCK.GT.1) THEN
          DO 35 IK = 2, NKEYCK
            IPNT   = IPNT + ISTP
            IF (KEYNCK(MOBJCD).LT.IQ(IPNT+IDHKSN))
     +        KEYNCK(MOBJCD) = IQ(IPNT+IDHKSN)
   35     CONTINUE
        ENDIF
      ENDIF
*
*  ** Fill up the remaining keys
*
      KEYNCK(MPSRCD)   = NK + 1
      KEYNCK(MXKPCD)   = MXK
      DO 40 I = 1, NPARCD
        KEYNCK(NOF1CK+2*I-1) = IBIGCD
        KEYNCK(NOF1CK+2*I)   = 0
   40 CONTINUE
      KEYNCK(IDHFLG)   = MSBIT1 (KEYNCK(IDHFLG), JPRTCD)
      IF (KEY7.EQ.0) THEN
        CALL DATIME (IDATE, ITIME)
        CALL CDPKTM (IDATE, ITIME, KEYNCK(IDHINS), IRC)
      ELSE
        KEYNCK(IDHINS) = KEY7
      ENDIF
      IF (NWKEY.GT.NSYS) THEN
        DO 50 IK = NSYS+1, NWKEY
          IF (CHFOR(IK:IK).EQ.'A'.OR.CHFOR(IK:IK).EQ.'H') THEN
            CALL UCTOH ('    ', KEYNCK(IK), 4, 4)
          ELSE
            KEYNCK(IK) = 0
          ENDIF
   50   CONTINUE
      ENDIF
*
*  ** Insert a keyname for the new partition
*
      CALL RZCDIR (PATHN, ' ')
      IF (IOPSCD.NE.0) THEN
        IF (PATHL.EQ.' ') THEN
          PATHL  = PATHN
          CALL RZLOCK ('CDMKDI')
        ENDIF
      ENDIF
      IF (LSTRCL(3).NE.0) CALL MZDROP (IDISCD, LSTRCL(3), ' ')
      CALL CDBANK (IDISCD, LSTRCL(3), LSTRCL(3), JBIAS, 'SAME', 0, 0, 0,
     +             2, 0, IRC)
      IF (IRC.NE.0)                GO TO 95
      CALL RZOUT (IDISCD, LSTRCL(3), KEYNCK, ICYCLE, 'S')
      CALL MZDROP (IDISCD, LSTRCL(3), ' ')
      LSTRCL(3) = 0
      CALL CDPATH (TOP1CT, KEYNCK(MPSRCD))
      CALL RZMDIR (TOP1CT, NWKEY, CHFOR, CHTAG)
      IF (IQUEST(1).NE.0) THEN
        CALL RZDELK (KEYNCK, ICYCLE, 'C')
        GO TO 90
      ENDIF
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
      IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
      GO TO 95
*
* *** Error Messages
*
   90 IRC        = 48
      IQUEST(11) = ISTCH
      IQUEST(12) = NCH
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
      IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDMKDI : Error in '//
     +   'Directory Search - ISTCH = '',I5,'' NCH = '',I5)', IQUEST(11),
     +   2)
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Unlock the directory if required
*
   95 IF (PATHL.NE.' ') THEN
        CALL RZCDIR (PATHL, ' ')
        CALL RZFREE ('CDMKDI')
      ENDIF
#endif
*                                                             END CDMKDI
  999 END
