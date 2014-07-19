*
* $Id: cdplti.F,v 1.1.1.1 1996/02/28 16:24:23 mclareni Exp $
*
* $Log: cdplti.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:23  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB_IBM))
* Ignoring t=dummy
#endif
      SUBROUTINE CDPLTI (PATHN, MASK, KEYS, CHOPT, IRC)
*     =================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPLTI (PATHN, MASK, KEYS, CHOPT, IRC*)                 *
*                                                                      *
*   Plots the validity time of data objects in a given directory.      *
*   It works only when there is one pair of validity range for DB.     *
*   Selects objects with validity range as specified in KEYS(NOF1CK..) *
*   if the proper masks are used. It sees the object in data base has  *
*   lower range smaller than and higher range larger than the value    *
*   specified. Selection on insertion time demands data base object    *
*   to be inserted before the value supplied in selection              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     MASK     Integer vector indicating which elements of KEYS are    *
*              significant for selection. If MASK corresponding to     *
*              one of the fields of 'Beginning' validity range is set, *
*              it will select objects with start validity larger than  *
*              those requested in KEYS. If MASK corresponding to one   *
*              of the fields of 'End' validity range is set, it will   *
*              select objects with start validity smaller than those   *
*              in the KEYS vector (in the fields corresponding to end  *
*              validity). If MASK corresponding to time of insertion   *
*              is set, objects inserted earlier than KEYS(IDHINS) are  *
*              selected                                                *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     CHOPT    Character string with any of the following characters   *
*          X   Assumes validity range packed time ala CDPKTS           *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by Interactive interface                                    *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =161 : Illegal path name                               *
*               =162 : No keys or data in the directory                *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/cplots.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (JBIAS=2)
      LOGICAL         HEXIST
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*80
      DIMENSION       KEYS(9), MASK(9), ITIME(MXPACD), XU(2), YU(2)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                                        GO TO 999
      CALL UCOPY (MASK, IOKYCA, MXDMCK)
*
* *** Load the top level directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                                        GO TO 999
      IF (NPARCD.NE.1)                                     GO TO 999
*
      PATHY  = PAT1CT
      NCHR   = LENOCC (PATHY)
      IF (NKEYCK.LE.0) THEN
        IRC    = 162
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLOB : No keys'//
     +  ' in directory '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
      CALL CDKEYT
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
*
* *** Prepare a temporary storage for useful information
*
      CALL DATIME (IDATX, ITIMX)
      CALL CDPKTS (IDATX, ITIMX*100, INOWS, IRC)
      NPLM   = NPLMCP
      IF (IOPTP.EQ.0) THEN
        IF (NKEYCK.LT.NPLM) NPLM = NKEYCK
      ENDIF
      ND     = 3 * NPLM
      IF (LAUXCL(10).NE.0) CALL MZDROP (IDISCD, LAUXCL(10), 'L')
      CALL CDBANK (IDISCD, LAUXCL(10), LAUXCL(10), 2, 'TEMP', 0, 0, ND,
     +             2, -1, IRC)
      IF (IRC.NE.0)                                        GO TO 999
*
* *** Load useful data in the temporary bank
*
      IMIN   = IBIGCD
      IMAX   = -1
      NPL    = 0
      IKMAX  = 0
      IPRBCA = ISIGN (IPRBCA, -1)
      IPRECA = ISIGN (IPRECA, -1)
      IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
        IFLG   = 99
      ELSE
        IFLG   = 0
      ENDIF
      DO 10 I = 1, NPARCD
        ITIME(I) = 1
   10 CONTINUE
      IF (IOPTP.EQ.0) THEN
        DO 15 JK = 1, NKEYCK
          IK     = NKEYCK + 1 - JK
          CALL CDKEYR (IK, NWKYCK, KEYVCK)
          CALL CDKSEL (ITIME, KEYS, KEYVCK, IFLG, ISEL, INBR)
          IF (ISEL.NE.0)                                   GO TO 15
          IF (IOPXCA.NE.0.AND.KEYVCK(NOF1CK+1).GT.INOWS)   GO TO 15
          NPL    = NPL + 1
          IF (NPL.LE.NPLM) THEN
            IPNT   = KOFUCD + LAUXCL(10) + (NPL - 1) * 3
            IQ(IPNT+1) = KEYVCK(IDHKSN)
            IQ(IPNT+2) = KEYVCK(NOF1CK+2*NPARCD-1)
            IQ(IPNT+3) = KEYVCK(NOF1CK+2*NPARCD)
            IF (KEYVCK(NOF1CK+2*NPARCD)  .GT.IMAX)
     +        IMAX  = KEYVCK(NOF1CK+2*NPARCD)
            IF (KEYVCK(NOF1CK+2*NPARCD-1).LT.IMIN)
     +        IMIN  = KEYVCK(NOF1CK+2*NPARCD-1)
            IF (IKMAX.LT.KEYVCK(IDHKSN)) IKMAX = KEYVCK(IDHKSN)
          ENDIF
   15   CONTINUE
*
      ELSE
        KST    = NWKYCK + 1
        NKEYS  = NKEYCK
        DO 30 JKK = 1, NKEYS
          IKK    = NKEYS + 1 - JKK
          KPNT   = IUHUNT (IKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*KST, KST)
          IF (KPNT.GT.0) THEN
            KPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            KPNT   = KOFSCD + LCDRCD + IKDRCD + (IKK - 1) * KST
          ENDIF
          CALL CDPSEL (ITIME, KEYS, IQ(KPNT+1), IFLG, ISEL)
          IF (ISEL.NE.0)                                   GO TO 30
          IF (IOPXCA.NE.0.AND.IQ(KPNT+NOF1CK+1).GT.INOWS)  GO TO 30
*
          CALL CDPATH (TOP1CT, IKK)
          PAT2CT = PATHY(1:NCHR)//'/'//TOP1CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            CALL MZDROP (IDISCD, LAUXCL(10), ' ')
            IRC    = 161
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLTI : Ill'//
     +      'egal pathname '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          DO 25 JK = 1, NKEYCK
            IK     = NKEYCK + 1 - JK
            CALL CDKEYR (IK, NWKYCK, KEYVCK)
            CALL CDKSEL (ITIME, KEYS, KEYVCK, IFLG, ISEL, INBR)
            IF (ISEL.NE.0)                                 GO TO 25
            IF (IOPXCA.NE.0.AND.KEYVCK(NOF1CK+1).GT.INOWS) GO TO 25
            NPL    = NPL + 1
            IF (NPL.LE.NPLM) THEN
              IPNT   = KOFUCD + LAUXCL(10) + (NPL - 1) * 3
              IQ(IPNT+1) = KEYVCK(IDHKSN)
              IQ(IPNT+2) = KEYVCK(NOF1CK+2*NPARCD-1)
              IQ(IPNT+3) = KEYVCK(NOF1CK+2*NPARCD)
              IF (KEYVCK(NOF1CK+2*NPARCD)  .GT.IMAX)
     +          IMAX  = KEYVCK(NOF1CK+2*NPARCD)
              IF (KEYVCK(NOF1CK+2*NPARCD-1).LT.IMIN)
     +          IMIN  = KEYVCK(NOF1CK+2*NPARCD-1)
              IF (IKMAX.LT.KEYVCK(IDHKSN)) IKMAX = KEYVCK(IDHKSN)
            ENDIF
   25     CONTINUE
          CALL RZCDIR (PATHY, ' ')
          IF (IQUEST(1).NE.0) THEN
            CALL MZDROP (IDISCD, LAUXCL(10), ' ')
            IRC    = 161
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLTI : Ill'//
     +      'egal pathname '//PATHY//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          IF (NPL.GT.NPLM)                                 GO TO 35
   30   CONTINUE
*
      ENDIF
*
* *** All points collected; now decide the scale and header
*
   35 IF (NPL.EQ.0) THEN
        CALL MZDROP (IDISCD, LAUXCL(10), ' ')
        GO TO 999
      ENDIF
      IF (NPL.GT.NPLM) THEN
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          IARGCD(1) = NPL
          IARGCD(2) = NPLM
          CALL CDPRNT (LPRTCD, '(/,'' CDPLTI : Too many objects '','//
     +         'I6,'' only '',I6,'' shown'')', IARGCD, 2)
        ENDIF
#endif
        NPL    = NPLM
      ENDIF
      IF (IOPXCA.NE.0) THEN
        IF (IMAX.GT.INOWS) IMAX = INOWS
        CALL CDUPTS (IDATX, ITIMX, IMIN, IRC)
        CALL CDTIMC (IDATX, ITIMX)
        IBINCP(3) = IDATX/10000 + 1900
        IBINCP(2) = MOD (IDATX/100, 100)
        IBINCP(1) = MOD (IDATX    , 100)
        CALL CALDAT (101, CHRPCP, IBINCP, IRET)
        DYSTCP = CHRPCP(26:34)
        IDAY1  = IBINCP(6)
        CALL CDUPTS (IDATX, ITIMX, IMAX, IRC)
        CALL CDTIMC (IDATX, ITIMX)
        IBINCP(3) = IDATX/10000 + 1900
        IBINCP(2) = MOD (IDATX/100, 100)
        IBINCP(1) = MOD (IDATX    , 100)
        CALL CALDAT (101, CHRPCP, IBINCP, IRET)
        DYENCP = CHRPCP(26:34)
        IDAY2  = IBINCP(6) - IDAY1 + 1
        TMAX   = 24.0 * IDAY2
      ELSE
        TMAX   = (IMAX - IMIN) + 1.
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
        WRITE (DYSTCP, '(I10)') IMIN
        WRITE (DYENCP, '(I10)') IMAX
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
        CALL UTWRIT (DYSTCP, '(I10)', IMIN, 1)
        CALL UTWRIT (DYENCP, '(I10)', IMAX, 1)
#endif
      ENDIF
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
      WRITE (CTTLCP, 2000) DYSTCP, DYENCP
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
      CALL UTWRIT (CITLDP, '(''Object Validity Range during '//DYSTCP//
     +     ' - '//DYENCP//''')', IARGCD, 0)
#endif
      IF (HEXIST(-101)) CALL HDELET (-101)
      CALL HBOOK1 (-101, CTTLCP, 2, 0., TMAX, 0.)
      CALL HMINIM (-101, 0.)
      CALL HMAXIM (-101, FLOAT(IKMAX)+2.)
      CALL HTITLE (PATHY)
      CALL HPLOPT ('HORI', 1)
      CALL HPLOT  (-101, ' ', 'HIST', 0)
*
* *** Now plot the keys
*
      DO 40 JK = 1, NPL
        IPNT   = KOFUCD + LAUXCL(10) + (JK - 1) * 3
        IK     = IQ(IPNT+1)
        IF (IOPXCA.NE.0) THEN
          CALL CDUPTS (IDATX, ITIMX, IQ(IPNT+2), IRC)
          CALL CDTIMC (IDATX, ITIMX)
          IBINCP(3) = IDATX/10000 + 1900
          IBINCP(2) = MOD (IDATX/100, 100)
          IBINCP(1) = MOD (IDATX    , 100)
          CALL CALDAT (101, CHRPCP, IBINCP, IRET)
          IDAY   = IBINCP(6) - IDAY1
          NHOUR  = ITIMX / 10000
          NMIN   = MOD (ITIMX/100, 100)
          NSEC   = MOD (ITIMX    , 100)
          HOURS  = NHOUR + FLOAT(NMIN)/60. + FLOAT(NSEC)/3600.
          XU(1)  = HOURS + 24.0 * IDAY
          CALL CDUPTS (IDATX, ITIMX, IQ(IPNT+3), IRC)
          CALL CDTIMC (IDATX, ITIMX)
          IBINCP(3) = IDATX/10000 + 1900
          IBINCP(2) = MOD (IDATX/100, 100)
          IBINCP(1) = MOD (IDATX    , 100)
          CALL CALDAT (101, CHRPCP, IBINCP, IRET)
          IDAY   = IBINCP(6) - IDAY1
          NHOUR  = ITIMX / 10000
          NMIN   = MOD (ITIMX/100, 100)
          NSEC   = MOD (ITIMX    , 100)
          HOURS  = NHOUR + FLOAT(NMIN)/60. + FLOAT(NSEC)/3600.
          XU(2)  = HOURS + 24.0 * IDAY
        ELSE
          XU(1)  = IQ(IPNT+2) - IMIN
          XU(2)  = IQ(IPNT+3) - IMIN
        ENDIF
        IF (XU(2).GT.TMAX) XU(2) = TMAX
        YU(1)  = FLOAT(IK)
        YU(2)  = YU(1)
        CALL HPLINE (XU, YU, 2, ' ')
   40 CONTINUE
*
   50 IF (LAUXCL(10).NE.0) CALL MZDROP (IDISCD, LAUXCL(10), ' ')
      IF (HEXIST(-101)) CALL HDELET (-101)
      IRC    = 0
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2000 FORMAT ('Object Validity Range during ',A,' - ',A)
#endif
*                                                             END CDPLTI
  999 END
