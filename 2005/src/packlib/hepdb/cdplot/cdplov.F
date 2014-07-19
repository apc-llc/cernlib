*
* $Id: cdplov.F,v 1.1.1.1 1996/02/28 16:24:22 mclareni Exp $
*
* $Log: cdplov.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:22  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB_IBM))
* Ignoring t=dummy
#endif
      SUBROUTINE CDPLOV (PATHN, MASK, KEYS, NOBJ, KOBJ, NST, CHOPT, IRC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPLOV (PATHN, MASK, KEYS, NOBJ, KOBJ, NST, CHOPT, IRC*)*
*                                                                      *
*   Plots data elemnet(s) versus data element(s) for a given path name *
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
*     NOBJ     Number of data objects to be plotted                    *
*     KOBJ     Vector specifying the element numbers to be plotted     *
*     NST      Step size for selection of object number                *
*     CHOPT    Character string with any of the following characters   *
*          L   line to be drawn through the points                     *
*          P   a symbol to be drawn at each point                      *
*              (If L or P not chosen, a default marker to be drawn     *
*               at each point)                                         *
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
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/cplots.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (JBIAS=2)
      LOGICAL         HEXIST
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*80
      DIMENSION       KEYS(9), MASK(9), KOBJ(2,9), ITIME(MXPACD)
      DOUBLE PRECISION DVAL
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
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLOV : No keys'//
     +  ' in directory '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      CALL CDKEYT
*
      IF (IOPXCA.EQ.0) THEN
        KABS   = IDHINS
      ELSE
        KABS   = NOF1CK + 1
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
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLOV : Illegal'//
     +  ' number of objects '',2I12)', IQUEST(11), 2)
#endif
        GO TO 999
      ENDIF
      NOBJD  = 0
      NOBM   = KOBJ(1,1)
      DO 5 IK = 1, NOBJ
        II     = IUCOMP (KOBJ(1,IK), KOBJCP, NOBJD)
        IF (II.EQ.0) THEN
          NOBJD  = NOBJD + 1
          II     = NOBJD
          KOBJCP(II) = KOBJ(1,IK)
          IF (KOBJCP(II).GT.NOBM) NOBM = KOBJCP(II)
        ENDIF
        INDXCP(1,IK) = II
        II     = IUCOMP (KOBJ(2,IK), KOBJCP, NOBJD)
        IF (II.EQ.0) THEN
          NOBJD  = NOBJD + 1
          II     = NOBJD
          KOBJCP(II) = KOBJ(2,IK)
        ENDIF
        INDXCP(2,IK) = II
    5 CONTINUE
*
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      NTOT   = 0
#if defined(CERNLIB__DEBUG)
      NBAD   = 0
#endif
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
      ND     = (NOBJD + 1) * NPLM
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
          CALL VZERO (KEYVCK, NWKYCK)
          KEYVCK(IDHKSN) = IK
          IOKYCA(IDHKSN) = 1
          CALL CDKXIN (ITIME, IDISCD, LAUXCL(9), LAUXCL(9), JBIAS,
     +                 NWKYCK, KEYVCK, IPREC, IRC)
          IOKYCA(IDHKSN) = 0
          IF (IRC.NE.0) THEN
            IF (LAUXCL(9).NE.0) CALL MZDROP (IDISCD, LAUXCL(9), 'L')
            IRC    = 0
#if defined(CERNLIB__DEBUG)
            NBAD   = NBAD + 1
#endif
            GO TO 20
          ENDIF
          IF (NOBM.GT.IQ(KOFUCD+LAUXCL(9)-1)) THEN
#if defined(CERNLIB__DEBUG)
            NBAD   = NBAD + 1
#endif
            CALL MZDROP (IDISCD, LAUXCL(9), 'L')
            GO TO 20
          ENDIF
          IF (NPL.EQ.0) CALL CDIOTY (LAUXCL(9), NOBJD, KOBJCP, IOTYCP)
          NPL    = NPL + 1
          IF (NPL.LE.NPLM) THEN
            IPNT   = KOFUCD + LAUXCL(10) + (NPL - 1) * (NOBJD + 1)
            IQ(IPNT+1) = KTIME
            DO 15 I = 1, NOBJD
              IF (IOTYCP(I).EQ.1.OR.IOTYCP(I).EQ.2.OR.IOTYCP(I).EQ.5)
     +                                                     THEN
                IVAL   = IQ(KOFUCD+LAUXCL(9)+KOBJCP(I))
                Q(IPNT+I+1) = IVAL
              ELSE IF (IOTYCP(I).EQ.4) THEN
                CALL UCOPY (Q(KOFUCD+LAUXCL(9)+KOBJCP(I)), DVAL, 2)
                Q(IPNT+I+1) = DVAL
              ELSE
                Q(IPNT+I+1) = Q(KOFUCD+LAUXCL(9)+KOBJCP(I))
              ENDIF
              IF (Q(IPNT+I+1).GT.VMAXCP) VMAXCP = Q(IPNT+I+1)
              IF (Q(IPNT+I+1).LT.VMINCP) VMINCP = Q(IPNT+I+1)
   15       CONTINUE
            IF (KTIME.LT.IMIN) IMIN = KTIME
            IF (KTIME.GT.IMAX) IMAX = KTIME
          ENDIF
          CALL MZDROP (IDISCD, LAUXCL(9), ' ')
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
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLOV : Ill'//
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
            NTOT   = NTOT + 1
            IF (NSTEP.GT.1) THEN
              IF (MOD(NTOT,NSTEP).NE.1)                      GO TO 35
            ENDIF
            LAUXCL(9) = 0
            KTIME  = KEYVCK(KABS)
            CALL VZERO (KEYVCK, NWKYCK)
            KEYVCK(IDHKSN) = IK
            IOKYCA(IDHKSN) = 1
            CALL CDKXIN (ITIME, IDISCD, LAUXCL(9), LAUXCL(9), JBIAS,
     +                   NWKYCK, KEYVCK, IPREC, IRC)
            IOKYCA(IDHKSN) = 0
            IF (IRC.NE.0) THEN
              IF (LAUXCL(9).NE.0) CALL MZDROP (IDISCD, LAUXCL(9), 'L')
              IRC    = 0
#if defined(CERNLIB__DEBUG)
              NBAD   = NBAD + 1
#endif
              GO TO 35
            ENDIF
            IF (NOBM.GT.IQ(KOFUCD+LAUXCL(9)-1)) THEN
#if defined(CERNLIB__DEBUG)
              NBAD   = NBAD + 1
#endif
              CALL MZDROP (IDISCD, LAUXCL(9), 'L')
              GO TO 35
            ENDIF
            IF (NPL.EQ.0) CALL CDIOTY (LAUXCL(9), NOBJD, KOBJCP, IOTYCP)
            NPL    = NPL + 1
            IF (NPL.LE.NPLM) THEN
              IPNT   = KOFUCD + LAUXCL(10) + (NPL - 1) * (NOBJD + 1)
              IQ(IPNT+1) = KTIME
              DO 30 I = 1, NOBJD
                IF (IOTYCP(I).EQ.1.OR.IOTYCP(I).EQ.2.OR.IOTYCP(I).EQ.5)
     +                                                     THEN
                  IVAL   = IQ(KOFUCD+LAUXCL(9)+KOBJCP(I))
                  Q(IPNT+I+1) = IVAL
                ELSE IF (IOTYCP(I).EQ.4) THEN
                  CALL UCOPY (Q(KOFUCD+LAUXCL(9)+KOBJCP(I)), DVAL, 2)
                  Q(IPNT+I+1) = DVAL
                ELSE
                  Q(IPNT+I+1) = Q(KOFUCD+LAUXCL(9)+KOBJCP(I))
                ENDIF
                IF (Q(IPNT+I+1).GT.VMAXCP) VMAXCP = Q(IPNT+I+1)
                IF (Q(IPNT+I+1).LT.VMINCP) VMINCP = Q(IPNT+I+1)
   30         CONTINUE
              IF (KTIME.LT.IMIN) IMIN = KTIME
              IF (KTIME.GT.IMAX) IMAX = KTIME
            ENDIF
            CALL MZDROP (IDISCD, LAUXCL(9), ' ')
   35     CONTINUE
          CALL RZCDIR (PATHY, ' ')
          IF (IQUEST(1).NE.0) THEN
            CALL MZDROP (IDISCD, LAUXCL(10), ' ')
            IRC    = 161
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPLOV : Ill'//
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
      IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDPLOV : Number of'//
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
          IARGCD(1) = NPLM
          CALL CDPRNT (LPRTCD, '(/,'' CDPLOV : Too many objects '','//
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
        IQ(KOFUCD+LAUXCL(9)+IK) = IQ(KOFUCD+LAUXCL(10)+(IK-1)*(NOBJD+1)
     +                              +1)
   50 CONTINUE
      CALL SORTZV (IQ(KOFUCD+LAUXCL(9)+1), IQ(KOFUCD+LAUXCL(9)+NPL+1),
     +             NPL, -1, 0, 0)
*
* *** Set up the header
*
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
      CALL HTITLE (PATHY)
      CALL HPLOPT ('HORI', 1)
*
* *** Now make the plots
*
      DO 70 JK = 1, NOBJ
        IK1    = INDXCP(1,JK)
        IK2    = INDXCP(2,JK)
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
        WRITE (CTTLCP, 2000) KOBJCP(IK2), KOBJCP(IK1), DYSTCP, DYENCP
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
        IARGCD(1) = KOBJCP(IK2)
        IARGCD(2) = KOBJCP(IK1)
        CALL UTWRIT (CTTLCP, '(''Object '',I5,'' vs. Object '',I5,'' '//
     +       '    '//DYSTCP//' - '//DYENCP//''')', IARGCD, 2)
#endif
        XRAN   = 0.1 * (VMAXCP - VMINCP)
        IF (XRAN.EQ.0.0) XRAN = 0.01
        XMIN   = VMINCP - XRAN
        XMAX   = VMAXCP + XRAN
        YRAN   = 0.1 * (VMAXCP - VMINCP)
        IF (YRAN.EQ.0.0) YRAN = 0.01
        YMIN   = VMINCP - YRAN
        YMAX   = VMAXCP + YRAN
        IF (HEXIST(-101)) CALL HDELET (-101)
        CALL HBOOK1 (-101, CTTLCP, 2, XMIN, XMAX, 0.)
        CALL HMINIM (-101, YMIN)
        CALL HMAXIM (-101, YMAX)
        DO 60 IK = 1, NPL
          IPNT   = IQ(KOFUCD+LAUXCL(9)+NPL+IK)
          KPNT   = KOFUCD + LAUXCL(10) + (IPNT - 1) * (NOBJD + 1) + 1
          Q(KOFUCD+LAUXCL(9)+2*NPL+IK) = Q(KPNT+IK1)
          Q(KOFUCD+LAUXCL(9)+3*NPL+IK) = Q(KPNT+IK2)
   60   CONTINUE
        CALL HPLOT  (-101, ' ', 'HIST', 0)
        IF (IOPPCA.NE.0) THEN
          CALL HPLSYM (Q(KOFUCD+LAUXCL(9)+2*NPL+1),
     +                 Q(KOFUCD+LAUXCL(9)+3*NPL+1), NPL, 31, 0.1, ' ')
        ELSE IF (IOPLCA.NE.0) THEN
          CALL HPLINE (Q(KOFUCD+LAUXCL(9)+2*NPL+1),
     +                 Q(KOFUCD+LAUXCL(9)+3*NPL+1), NPL, ' ')
        ELSE
          CALL HPLSYM (Q(KOFUCD+LAUXCL(9)+2*NPL+1),
     +                 Q(KOFUCD+LAUXCL(9)+3*NPL+1), NPL, 24, 0.05, ' ')
        ENDIF
        IF (JK.NE.NOBJ) CALL CDUSIN
   70 CONTINUE
*
   80 IF (LAUXCL(10).NE.0) CALL MZDROP (IDISCD, LAUXCL(10), ' ')
      IF (LAUXCL(9) .NE.0) CALL MZDROP (IDISCD, LAUXCL(9),  ' ')
      IF (HEXIST(-101)) CALL HDELET (-101)
      IRC    = 0
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2000 FORMAT ('Object ',I5,' vs. Object ',I5,'     ',A,' - ',A)
#endif
*                                                             END CDPLOV
  999 END
