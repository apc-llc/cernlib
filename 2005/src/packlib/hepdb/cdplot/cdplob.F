*
* $Id: cdplob.F,v 1.1.1.1 1996/02/28 16:24:22 mclareni Exp $
*
* $Log: cdplob.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:22  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB_IBM))
* Ignoring t=dummy
#endif
      SUBROUTINE CDPLOB (PATHN,MASK,KEYS, NOBJ,KOBJ,KEX,NST,CHOPT,IRC)
*     ================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPLOB (PATHN, MASK,KEYS, NOBJ,KOBJ,KEX,NST,CHOPT, IRC*)*
*                                                                      *
*   Plots data elemnet(s) versus a key element for a given path name   *
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
*     MASK     Integer vector indicating which elements of KEYS are    *
*              significant for selection                               *
*     NOBJ     Number of data objects to be plotted                    *
*     KOBJ     Vector specifying the element numbers to be plotted     *
*     KEX      Key index for the abcissa                               *
*     NST      Step size for selection of object number                *
*     CHOPT    Character string with any of the following characters   *
*          L   a line to be drawn through the points                   *
*              (needed only when symbol and line both to be drawn)     *
*          P   a symbol to be drawn at each point                      *
*              (Default is a line to be drawn through the points)      *
*          S   all elements shown on the same plot                     *
*              (Default is a seperate plot for each variable)          *
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
*               =163 : Illegal number of objects                       *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/cplots.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (MXSYM=12, JBIAS=2)
      LOGICAL         HEXIST
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*255, CTAG*16, CEXT*5
      DIMENSION       KEYS(9),MASK(9),KOBJ(9),ITIME(MXPACD),ISYMB(MXSYM)
      DOUBLE PRECISION DVAL
      DATA            ISYMB /20,21,22,23,24,25,26,27,28,29,30,31/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                                    GO TO 999
      CALL UCOPY (MASK, IOKYCA, MXDMCK)
*
* *** Load the top level directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                                    GO TO 999
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
      CALL CDKYTG
*
      IF ((KEX.GT.NOF1CK.AND.KEX.LE.NOF1CK+2*NPARCD).OR.KEX.EQ.IDHINS)
     +                                                              THEN
        KABS   = KEX
      ELSE
        KABS   = IDHINS
      ENDIF
      IF (NST.GT.0) THEN
        NSTEP  = NST
      ELSE
        NSTEP  = 1
      ENDIF
      IF (NOBJ.LT.1.OR.NOBJ.GT.NOBMCP) THEN
        IRC        = 163
        IQUEST(11) = NOBJ
        IQUEST(12) = NOBMCP
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLOB : Illegal'//
     +  ' number of objects '',2I12)', IQUEST(11), 2)
#endif
        GO TO 999
      ENDIF
      NOBM   = KOBJ(1)
      DO 5 I = 2, NOBJ
        IF (KOBJ(I).GT.NOBM) NOBM = KOBJ(I)
    5 CONTINUE
*
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      NTOT   = 0
      IF (KEX.GT.0.AND.KEX.LE.NWKYCK) THEN
        KPEX   = KEX
      ELSE
        KPEX   = IDHINS
      ENDIF
      CTAG   = CTAGCK(KPEX)
*
* *** Prepare a temporary storage for useful information
*
      CALL DATIME (IDATX, ITIMX)
      CALL CDPKTS (IDATX, ITIMX*100, INOWS, IRC)
      IF (KABS.EQ.IDHINS) THEN
        CALL CDPKTM (IDATX, ITIMX, ITNOW, IRC)
      ELSE
        ITNOW  = INOWS
      ENDIF
      NPLM   = NPLMCP
      IF (IOPTP.EQ.0) THEN
        IF (NKEYCK.LT.NPLM) NPLM = NKEYCK
      ENDIF
      ND     = (NOBJ + 1) * NPLM
#if defined(CERNLIB__DEBUG)
      NBAD   = 0
#endif
      IF (LAUXCL(10).NE.0) CALL MZDROP (IDISCD, LAUXCL(10), 'L')
      CALL CDBANK (IDISCD, LAUXCL(10), LAUXCL(10), 2, 'TEMP', 0, 0, ND,
     +             0, -1, IRC)
      IF (IRC.NE.0)                                    GO TO 999
      VMINCP = 9.0E20
      VMAXCP =-9.0E20
      IF (LAUXCL(9).NE.0) THEN
        CALL MZDROP (IDISCD, LAUXCL(9), 'L')
        LAUXCL(9) = 0
      ENDIF
*
* *** Load useful data in the temporary bank
*
      IMIN   = IBIGCD
      IMAX   = -1
      NPL    = 0
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
        DO 20 JK = 1, NKEYCK
          IK     = NKEYCK + 1 - JK
          CALL CDKEYR (IK, NWKYCK, KEYVCK)
          CALL CDKSEL (ITIME, KEYS, KEYVCK, IFLG, ISEL, INBR)
          IF (ISEL.NE.0)                                     GO TO 20
          IF (IOPXCA.NE.0.AND.KEYVCK(NOF1CK+1).GT.INOWS)     GO TO 20
          NTOT   = NTOT + 1
          IF (NSTEP.GT.1) THEN
            IF (MOD(NTOT,NSTEP).NE.1)                        GO TO 20
          ENDIF
          LAUXCL(9) = 0
          KTIME  = KEYVCK(KABS)
          KYVAL  = KEYVCK(KPEX)
          CALL VZERO (KEYVCK, NWKYCK)
          KEYVCK(IDHKSN) = IK
          IOKYCA(IDHKSN) = 1
          CALL CDKXIN (ITIME, IDISCD, LAUXCL(9), LAUXCL(9), JBIAS,
     +                 NWKYCK, KEYVCK, IPREC, IRC)
          IOKYCA(IDHKSN) = 0
          IF (IRC.NE.0) THEN
            IF (LAUXCL(9).NE.0)
     +        CALL MZDROP (IDISCD, LAUXCL(9), 'L')
#if defined(CERNLIB__DEBUG)
            NBAD   = NBAD + 1
#endif
            IRC    = 0
            GO TO 20
          ENDIF
          IF (NOBM.GT.IQ(KOFUCD+LAUXCL(9)-1)) THEN
#if defined(CERNLIB__DEBUG)
            NBAD   = NBAD + 1
#endif
            CALL MZDROP (IDISCD, LAUXCL(9), 'L')
            GO TO 20
          ENDIF
          IF (NPL.EQ.0) CALL CDIOTY (LAUXCL(9), NOBJ, KOBJ, IOTYCP)
          NPL    = NPL + 1
          IF (NPL.LE.NPLM) THEN
            IPNT   = KOFUCD + LAUXCL(10) + (NPL - 1) * (NOBJ + 1)
            IQ(IPNT+1) = KYVAL
            DO 15 I = 1, NOBJ
              IF (IOTYCP(I).EQ.1.OR.IOTYCP(I).EQ.2.OR.IOTYCP(I).EQ.5)
     +                                                     THEN
                IVAL   = IQ(KOFUCD+LAUXCL(9)+KOBJ(I))
                Q(IPNT+I+1) = IVAL
              ELSE IF (IOTYCP(I).EQ.4) THEN
                CALL UCOPY (Q(KOFUCD+LAUXCL(9)+KOBJ(I)), DVAL, 2)
                Q(IPNT+I+1) = DVAL
              ELSE
                Q(IPNT+I+1) = Q(KOFUCD+LAUXCL(9)+KOBJ(I))
              ENDIF
              IF (Q(IPNT+I+1).GT.VMAXCP) VMAXCP = Q(IPNT+I+1)
              IF (Q(IPNT+I+1).LT.VMINCP) VMINCP = Q(IPNT+I+1)
   15       CONTINUE
            IF (KTIME.LT.IMIN) IMIN = KTIME
            IF (KTIME.GT.IMAX) IMAX = KTIME
          ENDIF
          CALL MZDROP (IDISCD, LAUXCL(9), 'L')
   20   CONTINUE
*
      ELSE
        KST    = NWKYCK + 1
        NKEYS  = NKEYCK
        DO 40 JKK = 1, NKEYS
          IKK    = NKEYS + 1 - JKK
          KPNT   = IUHUNT (IKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*KST, KST)
          IF (KPNT.GT.0) THEN
            KPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            KPNT   = KOFSCD + LCDRCD + IKDRCD + (IKK - 1) * KST
          ENDIF
          CALL CDPSEL (ITIME, KEYS, IQ(KPNT+1), IFLG, ISEL)
          IF (ISEL.NE.0)                                     GO TO 40
          IF (IOPXCA.NE.0.AND.IQ(KPNT+NOF1CK+1).GT.INOWS)    GO TO 40
*
          CALL CDPATH (TOP1CT, IKK)
          PAT2CT = PATHY(1:NCHR)//'/'//TOP1CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            CALL MZDROP (IDISCD, LAUXCL(10), ' ')
            IRC    = 161
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLOB : Ill'//
     +      'egal pathname '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          DO 35 JK = 1, NKEYCK
            IK     = NKEYCK + 1 - JK
            CALL CDKEYR (IK, NWKYCK, KEYVCK)
            CALL CDKSEL (ITIME, KEYS, KEYVCK, IFLG, ISEL, INBR)
            IF (ISEL.NE.0)                                   GO TO 35
            IF (IOPXCA.NE.0.AND.KEYVCK(NOF1CK+1).GT.INOWS)   GO TO 35
            IF (NWKYCK.GT.NSYSCK) THEN
              DO 25 I = NSYSCK+1, NWKYCK
                IF (IOKYCA(I).NE.0.AND.KEYVCK(I).NE.KEYS(I)) GO TO 35
   25         CONTINUE
            ENDIF
            NTOT   = NTOT + 1
            IF (NSTEP.GT.1) THEN
              IF (MOD(NTOT,NSTEP).NE.1)                      GO TO 35
            ENDIF
            LAUXCL(9) = 0
            KTIME  = KEYVCK(KABS)
            KYVAL  = KEYVCK(KPEX)
            CALL VZERO (KEYVCK, NWKYCK)
            KEYVCK(IDHKSN) = IK
            IOKYCA(IDHKSN) = 1
            CALL CDKXIN (ITIME, IDISCD, LAUXCL(9), LAUXCL(9), JBIAS,
     +                   NWKYCK, KEYVCK, IPREC, IRC)
            IOKYCA(IDHKSN) = 0
            IF (IRC.NE.0) THEN
              IF (LAUXCL(9).NE.0)
     +          CALL MZDROP (IDISCD, LAUXCL(9), 'L')
#if defined(CERNLIB__DEBUG)
              NBAD   = NBAD + 1
#endif
              IRC    = 0
              GO TO 35
            ENDIF
            IF (NOBM.GT.IQ(KOFUCD+LAUXCL(9)-1)) THEN
#if defined(CERNLIB__DEBUG)
              NBAD   = NBAD + 1
#endif
              CALL MZDROP (IDISCD, LAUXCL(9), 'L')
              GO TO 35
            ENDIF
            IF (NPL.EQ.0) CALL CDIOTY (LAUXCL(9), NOBJ, KOBJ, IOTYCP)
            NPL    = NPL + 1
            IF (NPL.LE.NPLM) THEN
              IPNT   = KOFUCD + LAUXCL(10) + (NPL - 1) * (NOBJ + 1)
              IQ(IPNT+1) = KYVAL
              DO 30 I = 1, NOBJ
                IF (IOTYCP(I).EQ.1.OR.IOTYCP(I).EQ.2.OR.IOTYCP(I).EQ.5)
     +                                                     THEN
                  IVAL   = IQ(KOFUCD+LAUXCL(9)+KOBJ(I))
                  Q(IPNT+I+1) = IVAL
                ELSE IF (IOTYCP(I).EQ.4) THEN
                  CALL UCOPY (Q(KOFUCD+LAUXCL(9)+KOBJ(I)), DVAL, 2)
                  Q(IPNT+I+1) = DVAL
                ELSE
                  Q(IPNT+I+1) = Q(KOFUCD+LAUXCL(9)+KOBJ(I))
                ENDIF
                IF (Q(IPNT+I+1).GT.VMAXCP) VMAXCP = Q(IPNT+I+1)
                IF (Q(IPNT+I+1).LT.VMINCP) VMINCP = Q(IPNT+I+1)
   30         CONTINUE
              IF (KTIME.LT.IMIN) IMIN = KTIME
              IF (KTIME.GT.IMAX) IMAX = KTIME
            ENDIF
            CALL MZDROP (IDISCD, LAUXCL(9), 'L')
   35     CONTINUE
          CALL RZCDIR (PATHY, ' ')
          IF (IQUEST(1).NE.0) THEN
            CALL MZDROP (IDISCD, LAUXCL(10), ' ')
            IRC    = 161
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLOB : Ill'//
     +      'egal pathname '//PATHY//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          IF (NPL.GT.NPLM)                                   GO TO 45
   40   CONTINUE
*
      ENDIF
*
* *** All points collected
*
   45 CONTINUE
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDPLOB : Number of'//
     +   ' bad objects '',I12)', NBAD, 1)
#endif
      IF (NPL.EQ.0) THEN
        CALL MZDROP (IDISCD, LAUXCL(10), ' ')
        GO TO 999
      ENDIF
      IF (NPL.GT.NPLM) THEN
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          IARGCD(1) = NPL
          IARGCD(2) = NPLM
          CALL CDPRNT (LPRTCD, '(/,'' CDPLOB : Too many objects '','//
     +         'I6,'' only '',I6,'' shown'')', IARGCD, 2)
        ENDIF
#endif
        NPL    = NPLM
      ENDIF
*
* *** Sort the objects in increasing time
*
      CALL CDBANK (IDISCD, LAUXCL(9), LAUXCL(9), 2, 'TIME', 0, 0, 4*NPL,
     +             0, -1, IRC)
      IF (IRC.NE.0) THEN
        CALL MZDROP (IDISCD, LAUXCL(10), ' ')
        GO TO 999
      ENDIF
      DO 50 IK = 1, NPL
        IQ(KOFUCD+LAUXCL(9)+IK) = IQ(KOFUCD+LAUXCL(10)+(IK-1)*(NOBJ+1)
     +                              +1)
   50 CONTINUE
      CALL SORTZV (IQ(KOFUCD+LAUXCL(9)+1), IQ(KOFUCD+LAUXCL(9)+NPL+1),
     +             NPL, -1, 0, 0)
*
* *** Set up the header
*
      IF (KABS.EQ.IDHINS.OR.IOPXCA.NE.0) THEN
        IF (IMAX.GT.ITNOW) IMAX = ITNOW
        IF (KABS.EQ.IDHINS) THEN
          CALL CDUPTM (IDATX, ITIMX, IMIN, IRC)
          ITIMX  = ITIMX * 100
        ELSE
          CALL CDUPTS (IDATX, ITIMX, IMIN, IRC)
        ENDIF
        CALL CDTIMC (IDATX, ITIMX)
        IBINCP(3) = IDATX/10000 + 1900
        IBINCP(2) = MOD (IDATX/100, 100)
        IBINCP(1) = MOD (IDATX    , 100)
        CALL CALDAT (101, CHRPCP, IBINCP, IRET)
        DYSTCP = CHRPCP(26:34)
        IF (KABS.EQ.IDHINS) THEN
          CALL CDUPTM (IDATX, ITIMX, IMAX, IRC)
          ITIMX  = ITIMX * 100
        ELSE
          CALL CDUPTS (IDATX, ITIMX, IMAX, IRC)
        ENDIF
        CALL CDTIMC (IDATX, ITIMX)
        IBINCP(3) = IDATX/10000 + 1900
        IBINCP(2) = MOD (IDATX/100, 100)
        IBINCP(1) = MOD (IDATX    , 100)
        CALL CALDAT (101, CHRPCP, IBINCP, IRET)
        DYENCP = CHRPCP(26:34)
      ELSE
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
        WRITE (DYSTCP, '(I10)') IMIN
        WRITE (DYENCP, '(I10)') IMAX
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
        CALL UTWRIT (DYSTCP, '(I10)', IMIN, 1)
        CALL UTWRIT (DYENCP, '(I10)', IMAX, 1)
#endif
      ENDIF
      IDAY1  = 0
*
* *** Now setup the x-scale
*
      DO 55 IK = 1, NPL
        IPNT   = IQ(KOFUCD+LAUXCL(9)+NPL+IK)
        KYVAL  = IQ(KOFUCD+LAUXCL(9)+IPNT)
        IF (KPEX.EQ.IDHINS.OR.IOPXCA.NE.0) THEN
          IF (KPEX.EQ.IDHINS) THEN
            CALL CDUPTM (IDATX, ITIMX, KYVAL, IRC)
            ITIMX  = ITIMX * 100
          ELSE
            CALL CDUPTS (IDATX, ITIMX, KYVAL, IRC)
          ENDIF
          CALL CDTIMC (IDATX, ITIMX)
          NHOUR  = ITIMX / 10000
          NMIN   = MOD (ITIMX/100, 100)
          NSEC   = MOD (ITIMX    , 100)
          HOUR   = NHOUR + FLOAT(NMIN)/60.0 + FLOAT(NSEC)/3600.0
          IBINCP(3) = IDATX/10000 + 1900
          IBINCP(2) = MOD (IDATX/100, 100)
          IBINCP(1) = MOD (IDATX    , 100)
          CALL CALDAT (101, CHRPCP, IBINCP, IRET)
          IF (IK.EQ.1) IDAY1 = IBINCP(6)
          IDAY   = IBINCP(6) - IDAY1
          HOUR   = HOUR + 24.0 * IDAY
          Q(KOFUCD+LAUXCL(9)+2*NPL+IK) = HOUR
        ELSE
          Q(KOFUCD+LAUXCL(9)+2*NPL+IK) = KYVAL
        ENDIF
   55 CONTINUE
      IF (KPEX.EQ.IDHINS.OR.IOPXCA.NE.0) THEN
        XMIN   = Q(KOFUCD+LAUXCL(9)+2*NPL+1) - 0.1
        XMAX   = Q(KOFUCD+LAUXCL(9)+3*NPL)   + 0.1
      ELSE
        XMIN   = Q(KOFUCD+LAUXCL(9)+2*NPL+1) - 0.5
        XMAX   = Q(KOFUCD+LAUXCL(9)+3*NPL)   + 0.5
      ENDIF
      CALL HTITLE (PATHY)
*
* *** Now plot the object elements
*
      YRAN   = 0.1 * (VMAXCP - VMINCP)
      YMIN   = VMINCP - YRAN
      YMAX   = VMAXCP + YRAN
      IF (IOPSCA.NE.0) THEN
        CEXT   = ' ... '
        NCEXT  = 5
      ELSE
        CEXT   = ' '
        NCEXT  = 1
      ENDIF
      DO 70 IK = 1, NOBJ
        IF (IOPSCA.EQ.0.OR.IK.EQ.1) THEN
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
          WRITE (CTTLCP, 2001) KOBJ(IK),CEXT(1:NCEXT),CTAG,DYSTCP,DYENCP
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
          CALL UTWRIT (CITLCP, '(''Object '',I6,'''//CEXT(1:NCEXT)//
     +         'vs. '//CTAG//' '//DYSTCP//' - '//DYENCP//''')',
     +         KOBJ(IK), 1)
#endif
          IF (HEXIST(-101)) CALL HDELET (-101)
          CALL HBOOK1 (-101, CTTLCP, 2, XMIN, XMAX, 0.)
          CALL HPLOPT ('HORI', 1)
          CALL HMINIM (-101, YMIN)
          CALL HMAXIM (-101, YMAX)
          CALL HPLOT  (-101, ' ', 'HIST', 0)
        ENDIF
        KSYM   = MOD (IK, MXSYM)
        IF (KSYM.EQ.0) THEN
          ISYM   = ISYMB(MXSYM)
        ELSE
          ISYM   = ISYMB(KSYM)
        ENDIF
        DO 60 JK = 1, NPL
          IPNT   = IQ(KOFUCD+LAUXCL(9)+NPL+JK)
          KPNT   = (IPNT - 1) * (NOBJ + 1)
          Q(KOFUCD+LAUXCL(9)+3*NPL+JK) = Q(KOFUCD+LAUXCL(10)+KPNT+IK+1)
   60   CONTINUE
        IF (IOPPCA.NE.0) THEN
          CALL HPLSYM (Q(KOFUCD+LAUXCL(9)+2*NPL+1),
     +                 Q(KOFUCD+LAUXCL(9)+3*NPL+1), NPL, ISYM, 0.1, ' ')
        ENDIF
        IF (IOPPCA.EQ.0.OR.IOPLCA.NE.0) THEN
          CALL HPLINE (Q(KOFUCD+LAUXCL(9)+2*NPL+1),
     +                 Q(KOFUCD+LAUXCL(9)+3*NPL+1), NPL, ' ')
        ENDIF
        IF (IOPSCA.EQ.0.AND.IK.NE.NOBJ) CALL CDUSIN
   70 CONTINUE
*
   80 IF (LAUXCL(10).NE.0) CALL MZDROP (IDISCD, LAUXCL(10), ' ')
      IF (LAUXCL(9) .NE.0) CALL MZDROP (IDISCD, LAUXCL(9),  ' ')
      IF (HEXIST(-101)) CALL HDELET (-101)
      IRC    = 0
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT ('Object ',I6,A,'vs. ',A,' ',A,' - ',A)
#endif
*                                                             END CDPLOB
  999 END
