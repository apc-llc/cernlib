*
* $Id: cdntpl.F,v 1.1.1.1 1996/02/28 16:24:22 mclareni Exp $
*
* $Log: cdntpl.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:22  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDNTPL (NTUP, PATHS, NPATH, NOBJS, KOBJS, NKEYX,
     +                   KEYXS, MASK, KEYS, CHOPT, IRC)
*     ===========================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDNTPL (NTUP, PATHS, NPATH, NOBJS, KOBJS, NKEYX,        *
*                      KEYXS, MASK, KEYS, CHOPT, IRC*)                 *
*                                                                      *
*   Fills an N-tuple with data and key elements from multiple          *
*   directories as requested by the user.                              *
*   Selects objects with validity range as specified in KEYS(NOF1CK..) *
*   if the proper masks are used. It sees the object in data base has  *
*   lower range smaller than and higher range larger than the value    *
*   specified. Selection on insertion time demands data base object    *
*   to be inserted before the value supplied in selection              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     NTUP     Integer identifier of the ntuple                        *
*     PATHS    Character array describing the path names               *
*     NPATH    Number of paths given in PATHS                          *
*     NOBJS    Array giving the number of data elements to be stored   *
*              for PATHS(i)                                            *
*     KOBJS    Vector specifying the element indices to be stored for  *
*              PATHS(i)                                                *
*     NKEYX    Array giving the number of key elements to be stored    *
*              with data for PATHS(i)                                  *
*     KEYXS    Array giving key element indices to be stored with the  *
*              data. For a given path, data elements follow the key    *
*              elements                                                *
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
*              considered to contain useful information. The vector as *
*              well as CHOPT refers to the PATHS(1). Selections for    *
*              all subsequent paths is done on the basis of start      *
*              validity period from path 1 and a correlation table     *
*              between the user keys.                                  *
*     CHOPT    Character string with any of the following characters   *
*          T   Transforms the time keys (insertion and validity time)  *
*              to seconds/minutes passed since midnight January 1,1980 *
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
*               =164 : Illegal number of path names                    *
*               =165 : Illegal object element indices                  *
*               =166 : Illegal key element indices                     *
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
      CHARACTER       PATHY*80, PATHN*80, CTAG(25)*16
      CHARACTER*(*)   PATHS(*), CHOPT
      DIMENSION       KEYS(9), KEYXS(9), KOBJS(9), NKEYX(9), NOBJS(9)
      DIMENSION       MASK(9), KEYU(MXDMCK), ITIME(MXPACD)
      DOUBLE PRECISION DVAL
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Prepare object and key vector specifiers for different paths
*
      IF (NPATH.LT.1.OR.NPATH.GT.NPMXCP) THEN
        IRC        = 164
        IQUEST(11) = NPATH
        IQUEST(12) = NPMXCP
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL : Illegal'//
     +  ' number of path names '',2I10)', IQUEST(11), 2)
#endif
        GO TO 999
      ENDIF
*
      NDSMCP(1) = 0
      NKSMCP(1) = 0
      NKSTCP(1)  = 0
      DO 10 J = 1, NPATH
        IF (NKEYX(J).LT.0.OR.NOBJS(J).LT.0.OR.NKEYX(J)+NOBJS(J).LE.0)
     +                                                              THEN
          IRC        = 163
          IQUEST(11) = MIN0 (NKEYX(J), NOBJS(J))
          IQUEST(12) = NOBMCP
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) THEN
            PATHY  = PATHS(J)
            CALL CDPRNT (LPRTCD, '(/,'' CDNTPL : Illegal number of ob'//
     +           'ject '',2I9,'' for '//PATHY(1:40)//''')',IQUEST(11),2)
          ENDIF
#endif
          GO TO 999
        ENDIF
        IF (J.GT.1) THEN
          NDSMCP(J) = NDSMCP(J-1) + NOBJS(J-1)
          NKSMCP(J) = NKSMCP(J-1) + NKEYX(J-1)
          NKSTCP(J) = NDSTCP(J-1) + NOBJS(J-1)
        ENDIF
        NDSTCP(J)  = NKSTCP(J)    + NKEYX(J)
        IBEGCP(J)  = 0
        NDEMCP(J)  = 0
        DO 5 I = 1, NOBJS(J)
          IOBJ   = KOBJS(NDSMCP(J)+I)
          IF (IOBJ.LT.1) THEN
            IRC        = 165
            IQUEST(11) = IOBJ
            IQUEST(12) = I
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) THEN
              PATHY  = PATHS(J)
              CALL CDPRNT (LPRTCD, '(/,'' CDNTPL : Illegal data eleme'//
     +             'nt index '',2I9,'' for '//PATHY(1:40)//''')',
     +             IQUEST(11), 2)
            ENDIF
#endif
            GO TO 999
          ENDIF
          IF (IOBJ.GT.NDEMCP(J)) NDEMCP(J) = IOBJ
    5   CONTINUE
   10 CONTINUE
      NTOT   = NDSTCP(NPATH) + NOBJS(NPATH)
      IF (NTOT.LE.0.OR.NTOT.GT.NOBMCP) THEN
        IRC        = 163
        IQUEST(11) = NTOT
        IQUEST(12) = NOBMCP
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL : Illegal'//
     +  ' number of objects '',2I9,'' for All Paths'')', IQUEST(11), 2)
#endif
        GO TO 999
      ENDIF
      CALL VZERO (IOTYCP, NTOT)
      NPLM  = NPLMCP
      CALL HCDIR (PATHN, 'R')
*
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                                          GO TO 999
      CALL UCOPY (MASK, IOKYCA, MXDMCK)
*
* *** Find the keys on the basis of which selection is to be made
*
      DO 35 NPT = 1, NPATH
*
*  **   Suppress blanks from the pathname and set current directory
*
        CALL CDLDUP (PATHS(NPT), 0, IRC)
        IF (IRC.NE.0)                                        GO TO 999
        PATHY  = PAT1CT
        CALL CDKYTG
*
        IF (NKEYCK.LE.0) THEN
          IRC    = 162
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL : No '//
     +    'keys in directory '//PATHY//''')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
*
*  **   Load number of user keys to be tested for selection
*  **   for each path and their indices
*
        NSKYCP(NPT) = 0
        IF (NPT.EQ.1) THEN
          DO 15 I = NSYSCK+1, NWKYCK
            IF (IOKYCA(I).NE.0) THEN
              NSKYCP(NPT) = NSKYCP(NPT) + 1
              NKEYCP(NSKYCP(NPT),NPT) = I
              KKEYCP(NSKYCP(NPT),NPT) = I
              CTAG(NSKYCP(NPT))       = CTAGCK(I)
            ENDIF
   15     CONTINUE
        ELSE IF (NSKYCP(1).GT.0) THEN
          DO 25 I = 1, NSKYCP(1)
            DO 20 J = NSYSCK+1, NWKYCK
              IF (CTAG(I).EQ.CTAGCK(J)) THEN
                NSKYCP(NPT) = NSKYCP(NPT) + 1
                NKEYCP(NSKYCP(NPT),NPT) = J
                KKEYCP(NSKYCP(NPT),NPT) = NKEYCP(I,1)
                GO TO 25
              ENDIF
   20       CONTINUE
   25     CONTINUE
        ENDIF
*
*  **   Load the IO type of the keys
*
        DO 30 I = 1, NKEYX(NPT)
          IK     = KEYXS(NKSMCP(NPT)+I)
          IF (IK.LT.1.OR.IK.GT.NWKYCK) THEN
            IRC        = 166
            IQUEST(11) = I
            IQUEST(12) = IK
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL : Ill'//
     +      'egal key  element index'',2I9,'' for '//PATHY(1:40)//''')',
     +      IQUEST(11), 2)
#endif
            GO TO 999
          ENDIF
          IF (IOTYCK(IK).NE.6) THEN
            IOTYCP(NKSTCP(NPT)+I) = IOTYCK(IK)
          ELSE
            IOTYCP(NKSTCP(NPT)+I) = 5
          ENDIF
   30   CONTINUE
*
*  **   Find maximum number of objects from Path 1
*
        IF (NPT.EQ.1) THEN
          IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
          NPAIR  = NPARCD
          IF (IOPTP.EQ.0) THEN
            IF (NKEYCK.LT.NPLM) NPLM = NKEYCK
          ENDIF
        ENDIF
   35 CONTINUE
*
* *** Prepare temporary storage
*
      CALL DATIME (IDATX, ITIMX)
      CALL CDPKTS (IDATX, ITIMX*100, INOWS, IRC)
      ND    = (NTOT + NPAIR) * NPLM
      IF (LAUXCL(10).NE.0) CALL MZDROP (IDISCD, LAUXCL(10), 'L')
      CALL CDBANK (IDISCD, LAUXCL(10), LAUXCL(10), 2, 'TEMP', 0, 0, ND,
     +             0, -1, IRC)
      IF (IRC.NE.0)                                          GO TO 999
      NCUR   = 0
      NCURD  = 0
      NBAD   = 0
      NOLD   = 0
      NOLDD  = 0
*
* *** Load useful data in the temp bank from Path 1
*
   40 NPL    = 0
      CALL CDLDUP (PATHS(1), 0, IRC)
      IF (IRC.NE.0)                                          GO TO 999
      PATHY  = PAT1CT
      NCHR   = LENOCC (PATHY)
      CALL CDKEYT
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      IF (LAUXCL(9).NE.0) THEN
        CALL MZDROP (IDISCD, LAUXCL(9), 'L')
        LAUXCL(9) = 0
      ENDIF
      IPRBCA = ISIGN (IPRBCA, -1)
      IPRECA = ISIGN (IPRECA, -1)
      IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
        IFLG   = 99
      ELSE
        IFLG   = 0
      ENDIF
      DO 45 I = 1, NPAIR
        ITIME(I) = 1
   45 CONTINUE
      IF (NWKYCK.GT.NSYSCK) THEN
        DO 50 I = NSYSCK+1, NWKYCK
          IOKYCA(I) = MASK(I)
   50   CONTINUE
      ENDIF
*
      IF (IOPTP.EQ.0) THEN
*
*  **   For non-parttioned directories
*
        DO 70 IK = NCUR+1, NKEYCK
          CALL CDKEYR (IK, NWKYCK, KEYVCK)
*
*  **     Store the time values for the subsequent loop runs
*
          CALL CDKSEL (ITIME, KEYS, KEYVCK, IFLG, ISEL, INBR)
          IF (ISEL.NE.0)                                     GO TO 70
          IF (IOPXCA.NE.0.AND.KEYVCK(NOF1CK+1).GT.INOWS)     GO TO 70
          IF (NOBJS(1).GT.0) THEN
            LAUXCL(9) = 0
            CALL VZERO (KEYVCK, NWKYCK)
            KEYVCK(IDHKSN) = IK
            IOKYCA(IDHKSN) = 1
            CALL CDKXIN (ITIME, IDISCD, LAUXCL(9), LAUXCL(9), JBIAS,
     +                   NWKYCK, KEYVCK, IPREC, IRC)
            IOKYCA(IDHKSN) = 0
            IF (IRC.NE.0) THEN
              IF (LAUXCL(9).NE.0) CALL MZDROP (IDISCD, LAUXCL(9), 'L')
              IRC    = 0
              GO TO 70
            ENDIF
            IF (NDEMCP(1).GT.IQ(KOFUCD+LAUXCL(9)-1)) THEN
              NBAD   = NBAD +1
              CALL MZDROP (IDISCD, LAUXCL(9), 'L')
              GO TO 70
            ENDIF
            IF (IBEGCP(1).EQ.0) THEN
              IBEGCP(1) = 1
              CALL CDIOTY (LAUXCL(9), NOBJS(1), KOBJS(NDSMCP(1)+1),
     +                     IOTYCP(NDSTCP(1)+1))
            ENDIF
          ENDIF
          NPL    = NPL + 1
          IF (NPL.GT.NPLM) THEN
            NPL    = NPLM
            NCUR   = IK - 1
            GO TO 100
          ENDIF
*
*  **     Load the requested keys and the begin validity
*
          IPNT = KOFUCD + LAUXCL(10) + (NPL - 1) * (NTOT + NPAIR)
          DO 55 I = 1, NPAIR
            IQ(IPNT+I) = KEYVCK(NOF1CK+I)
   55     CONTINUE
          DO 60 I = 1, NKEYX(1)
            IND = KEYXS(NKSMCP(1)+I)
            IF (IOPTCA.NE.0.AND.IND.NE.IDHINS) THEN
              CALL CDTIMM (KEYVCK(IND), IVAL)
              Q(IPNT+NKSTCP(1)+I+NPAIR) = IVAL
            ELSE IF (IOPTCA.NE.0.AND.IOPXCA.NE.0.AND.(IND.GT.NOF1CK.AND.
     +               IND.LE.NOF1CK+2*NPARCD)) THEN
              CALL CDTIMS (KEYVCK(IND), IVAL)
              Q(IPNT+NKSTCP(1)+I+NPAIR) = IVAL
            ELSE
              Q(IPNT+NKSTCP(1)+I+NPAIR) = KEYVCK(IND)
            ENDIF
   60     CONTINUE
          DO 65 I = 1, NOBJS(1)
            ID     = NDSTCP(1) + I
            II     = KOBJS(NDSMCP(1)+I)
            IF (IOTYCP(ID).EQ.1.OR.IOTYCP(ID).EQ.2.OR.IOTYCP(ID).EQ.5)
     +                                THEN
              IVAL   = IQ(KOFUCD+LAUXCL(9)+II)
              Q(IPNT+ID+NPAIR) = IVAL
            ELSE IF (IOTYCP(ID).EQ.4) THEN
              CALL UCOPY (Q(KOFUCD+LAUXCL(9)+II), DVAL, 2)
              Q(IPNT+ID+NPAIR) = DVAL
            ELSE
              Q(IPNT+ID+NPAIR) = Q(KOFUCD+LAUXCL(9)+II)
            ENDIF
   65     CONTINUE
          CALL MZDROP (IDISCD, LAUXCL(9), 'L')
   70   CONTINUE
*
      ELSE
*
*   **  Partitioned directory
*
        KST    = NWKYCK + 1
        NKEYS  = NKEYCK
        NCUS   = NCUR
        DO 95 IKK = NCURD+1, NKEYS
          KPNT = IUHUNT (IKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                   NKEYS*KST, KST)
          IF (KPNT.GT.0) THEN
            KPNT  = KOFSCD + LCDRCD + IKDRCD +KPNT - MPSRCD
          ELSE
            KPNT  = KOFSCD + LCDRCD + IKDRCD + (IKK - 1) * KST
          ENDIF
          CALL CDPSEL (ITIME, KEYS, IQ(KPNT+1), IFLG, ISEL)
          IF (ISEL.NE.0)                                     GO TO 95
          IF (IOPXCA.NE.0.AND.IQ(KPNT+NOF1CK+1).GT.INOWS)    GO TO 95
*
          CALL CDPATH (TOP1CT, IKK)
          PAT2CT = PATHY(1:NCHR)//'/'//TOP1CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            CALL MZDROP (IDISCD, LAUXCL(10), 'L')
            IRC    = 161
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL :'//
     +      ' Illegal pathname '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
*
          DO 90 IK = NCUS+1, NKEYCK
            CALL CDKEYR (IK, NWKYCK, KEYVCK)
*
*  **       Store the time values for the subsequent loop runs
*
            CALL CDKSEL (ITIME, KEYS, KEYVCK, IFLG, ISEL, INBR)
            IF (ISEL.NE.0)                                   GO TO 90
            IF (IOPXCA.NE.0.AND.KEYVCK(NOF1CK+1).GT.INOWS)   GO TO 90
            IF (NOBJS(1).GT.0) THEN
              LAUXCL(9) = 0
              CALL VZERO (KEYVCK, NWKYCK)
              KEYVCK(IDHKSN) = IK
              IOKYCA(IDHKSN) = 1
              CALL CDKXIN (ITIME, IDISCD, LAUXCL(9), LAUXCL(9), JBIAS,
     +                     NWKYCK, KEYVCK, IPREC, IRC)
              IOKYCA(IDHKSN) = 0
              IF (IRC.NE.0) THEN
                IF (LAUXCL(9).NE.0) CALL MZDROP (IDISCD, LAUXCL(9), 'L')
                IRC    = 0
                GO TO 90
              ENDIF
              IF (NDEMCP(1).GT.IQ(KOFUCD+LAUXCL(9)-1)) THEN
                NBAD   = NBAD +1
                CALL MZDROP (IDISCD, LAUXCL(9), 'L')
                GO TO 90
              ENDIF
              IF (IBEGCP(1).EQ.0) THEN
                IBEGCP(1) = 1
                CALL CDIOTY (LAUXCL(9), NOBJS(1), KOBJS(NDSMCP(1)+1),
     +                       IOTYCP(NDSTCP(1)+1))
              ENDIF
            ENDIF
            NPL    = NPL + 1
            IF (NPL.GT.NPLM) THEN
              NPL    = NPLM
              NCUR   = IK - 1
              NCURD  = IKK - 1
              GO TO 100
            ENDIF
*
*  **       Load the requested keys and key(3)
*
            IPNT = KOFUCD + LAUXCL(10) + (NPL - 1) * (NTOT + NPAIR)
            DO 75 I = 1, NPAIR
              IQ(IPNT+I) = KEYVCK(NOF1CK+I)
   75       CONTINUE
            DO 80 I = 1, NKEYX(1)
              IND = KEYXS(NKSMCP(1)+I)
              IF (IOPTCA.NE.0.AND.IND.NE.IDHINS) THEN
                CALL CDTIMM (KEYVCK(IND), IVAL)
                Q(IPNT+NKSTCP(1)+I+NPAIR) = IVAL
              ELSE IF (IOPTCA.NE.0.AND.IOPXCA.NE.0.AND.(IND.GT.NOF1CK
     +                 .AND.IND.LE.NOF1CK+2*NPARCD)) THEN
                CALL CDTIMS (KEYVCK(IND), IVAL)
                Q(IPNT+NKSTCP(1)+I+NPAIR) = IVAL
              ELSE
                Q(IPNT+NKSTCP(1)+I+NPAIR) = KEYVCK(IND)
              ENDIF
   80       CONTINUE
            DO 85 I = 1, NOBJS(1)
              ID     = NDSTCP(1) + I
              II     = KOBJS(NDSMCP(1)+I)
              IF (IOTYCP(ID).EQ.1.OR.IOTYCP(ID).EQ.2.OR.IOTYCP(ID).EQ.5)
     +                                  THEN
                IVAL   = IQ(KOFUCD+LAUXCL(9)+II)
                Q(IPNT+ID+NPAIR) = IVAL
              ELSE IF (IOTYCP(ID).EQ.4) THEN
                CALL UCOPY (Q(KOFUCD+LAUXCL(9)+II), DVAL, 2)
                Q(IPNT+ID+NPAIR) = DVAL
              ELSE
                Q(IPNT+ID+NPAIR) = Q(KOFUCD+LAUXCL(9)+II)
              ENDIF
   85       CONTINUE
            CALL MZDROP (IDISCD, LAUXCL(9), 'L')
   90     CONTINUE
*
          NCUS   = 0
          CALL RZCDIR (PATHY, ' ' )
          IF (IQUEST(1).NE.0) THEN
            CALL MZDROP (IDISCD, LAUXCL(10), 'L')
            IRC    = 161
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL :'//
     +      ' Illegal pathname '//PATHY//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
   95   CONTINUE
      ENDIF
*
* ** Check if objects collected
*
  100 IF (NPL.EQ.0) THEN
        CALL MZDROP (IDISCD, LAUXCL(10), 'L')
        IRC    = 0
        GO TO 999
      ENDIF
*
* *** Loop over the secondary directories and fill the rest
*
      DO 105 I = 1, NSYSCK
        KEYU(I) = KEYS(I)
  105 CONTINUE
      DO 180 NPT = 2, NPATH
        CALL CDLDUP (PATHS(NPT), 0, IRC)
        IF (IRC.NE.0)                                        GO TO 999
        PATHY  = PAT1CT
        NCHR   = LENOCC (PATHY)
        CALL CDKEYT
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
        IF (NWKYCK.GT.NSYSCK) THEN
          DO 110 I = NSYSCK+1, NWKYCK
            IOKYCA(I) = 0
  110     CONTINUE
        ENDIF
        DO 115 I = 1, NSKYCP(NPT)
          IOKYCA(NKEYCP(I,NPT)) = 1
          KEYU  (NKEYCP(I,NPT)) = KEYS(KKEYCP(I,NPT))
  115   CONTINUE
        IPRBCA = 0
        IPRECA = 0
*
        DO 170 IO = 1, NPL
          IPNT   = KOFUCD + LAUXCL(10) + (IO - 1) * (NTOT + NPAIR)
          IF (IQ(IPNT+1).LT.0)                               GO TO 170
          DO 120 I = 1, NPAIR
            ITIME(I) = IQ(IPNT+I)
  120     CONTINUE
          IF (IOPTP.EQ.0) THEN
*
*  **       For non-parttioned directories
*
            DO 135 JK = 1, NKEYCK
              IK     = NKEYCK + 1 - JK
              CALL CDKEYR (IK, NWKYCK, KEYVCK)
*
*  **         Select on the basis of start validity from Path 1
*
              CALL CDKSEL (ITIME, KEYU, KEYVCK, 0, ISEL, INBR)
              IF (ISEL.NE.0)                                 GO TO 135
              IF (NOBJS(NPT).GT.0) THEN
                LAUXCL(9) = 0
                CALL VZERO (KEYVCK, NWKYCK)
                KEYVCK(IDHKSN) = IK
                IOKYCA(IDHKSN) = 1
                CALL CDKXIN (ITIME, IDISCD, LAUXCL(9), LAUXCL(9), JBIAS,
     +                       NWKYCK, KEYVCK, IPREC, IRC)
                IOKYCA(IDHKSN) = 0
                IF (IRC.NE.0) THEN
                  NBAD   = NBAD + 1
                  IF (LAUXCL(9).NE.0)
     +              CALL MZDROP (IDISCD, LAUXCL(9), 'L')
                  IRC    = 0
                  IPNT   = KOFUCD + LAUXCL(10) + (IO - 1)*(NTOT + NPAIR)
                  IQ(IPNT+1) = -1
                  GO TO 170
                ENDIF
                IF (NDEMCP(NPT).GT.IQ(KOFUCD+LAUXCL(9)-1)) THEN
                  NBAD   = NBAD +1
                  CALL MZDROP (IDISCD, LAUXCL(9), 'L')
                  IPNT   = KOFUCD + LAUXCL(10) + (IO - 1)*(NTOT + NPAIR)
                  IQ(IPNT+1) = -1
                  GO TO 170
                ENDIF
                IF (IBEGCP(NPT).EQ.0) THEN
                  IBEGCP(NPT) = 1
                  CALL CDIOTY (LAUXCL(9), NOBJS(NPT),
     +            KOBJS(NDSMCP(NPT)+1), IOTYCP(NDSTCP(NPT)+1))
                ENDIF
              ENDIF
*
*  **         Load the requested keys and data elements
*
              IPNT   = KOFUCD + LAUXCL(10) + (IO - 1) * (NTOT + NPAIR)
              DO 125 I = 1, NKEYX(NPT)
                IND = KEYXS(NKSMCP(NPT)+I)
                IF (IOPTCA.NE.0.AND.IND.NE.IDHINS) THEN
                  CALL CDTIMM (KEYVCK(IND), IVAL)
                  Q(IPNT+NKSTCP(NPT)+I+NPAIR) = IVAL
                ELSE IF (IOPTCA.NE.0.AND.IOPXCA.NE.0.AND.(IND.GT.NOF1CK
     +                  .AND.IND.LE.NOF1CK+2*NPARCD)) THEN
                  CALL CDTIMS (KEYVCK(IND), IVAL)
                  Q(IPNT+NKSTCP(NPT)+I+NPAIR) = IVAL
                ELSE
                  Q(IPNT+NKSTCP(NPT)+I+NPAIR) = KEYVCK(IND)
                ENDIF
  125         CONTINUE
              DO 130 I = 1, NOBJS(NPT)
                ID     = NDSTCP(NPT) + I
                II     = KOBJS(NDSMCP(NPT)+I)
                IF (IOTYCP(ID).EQ.1.OR.IOTYCP(ID).EQ.2.OR.
     +              IOTYCP(ID).EQ.5)      THEN
                  IVAL   = IQ(KOFUCD+LAUXCL(9)+II)
                  Q(IPNT+ID+NPAIR) = IVAL
                ELSE IF (IOTYCP(ID).EQ.4) THEN
                  CALL UCOPY (Q(KOFUCD+LAUXCL(9)+II), DVAL, 2)
                  Q(IPNT+ID+NPAIR) = DVAL
                ELSE
                  Q(IPNT+ID+NPAIR) = Q(KOFUCD+LAUXCL(9)+II)
                ENDIF
  130         CONTINUE
              IF (LAUXCL(9).NE.0) CALL MZDROP (IDISCD, LAUXCL(9), 'L')
              GO TO 170
  135       CONTINUE
            IPNT   = KOFUCD + LAUXCL(10) + (IO - 1) * (NTOT + NPAIR)
            IQ(IPNT+1) = -1
            GO TO 170
*
          ELSE
*
*   **      Partitioned directory
*
            KST    = NWKYCK + 1
            NKEYS  = NKEYCK
            DO 155 JKK = 1, NKEYS
              IKK  = NKEYS + 1 - JKK
              KPNT = IUHUNT (IKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                       NKEYS*KST, KST)
              IF (KPNT.GT.0) THEN
                KPNT  = KOFSCD + LCDRCD + IKDRCD +KPNT - MPSRCD
              ELSE
                KPNT  = KOFSCD + LCDRCD + IKDRCD + (IKK - 1) * KST
              ENDIF
              CALL CDPSEL (ITIME, KEYU, IQ(KPNT+1), 0, ISEL)
              IF (ISEL.NE.0)                                 GO TO 155
*
              CALL CDPATH (TOP1CT, IKK)
              PAT2CT = PATHY(1:NCHR)//'/'//TOP1CT
              CALL RZCDIR (PAT2CT, ' ')
              IF (IQUEST(1).NE.0) THEN
                CALL MZDROP (IDISCD, LAUXCL(10), 'L')
                IRC    = 161
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL :'//
     +          ' Illegal pathname '//PAT2CT//''')', IARGCD, 0)
#endif
                GO TO 999
              ENDIF
              NKEYCK = IQUEST(7)
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
*
              DO 150 JK = 1, NKEYCK
                IK     = NKEYCK + 1 -JK
                CALL CDKEYR (IK, NWKYCK, KEYVCK)
*
*  **           Select on the basis of start validity from Path 1
*
                CALL CDKSEL (ITIME, KEYU, KEYVCK, 0, ISEL, INBR)
                IF (ISEL.NE.0)                               GO TO 150
                IF (NOBJS(NPT).GT.0) THEN
                  LAUXCL(9) = 0
                  CALL VZERO (KEYVCK, NWKYCK)
                  KEYVCK(IDHKSN) = IK
                  IOKYCA(IDHKSN) = 1
                  CALL CDKXIN (ITIME, IDISCD, LAUXCL(9), LAUXCL(9),
     +                         JBIAS, NWKYCK, KEYVCK, IPREC, IRC)
                  IOKYCA(IDHKSN) = 0
                  IF (IRC.NE.0) THEN
                    NBAD   = NBAD + 1
                    IF (LAUXCL(9).NE.0)
     +                CALL MZDROP (IDISCD, LAUXCL(9), 'L')
                    IRC    = 0
                    IPNT   = KOFUCD + LAUXCL(10) + (IO -1)*(NTOT +NPAIR)
                    IQ(IPNT+1) = -1
                    GO TO 160
                  ENDIF
                  IF (NDEMCP(NPT).GT.IQ(KOFUCD+LAUXCL(9)-1)) THEN
                    NBAD   = NBAD +1
                    CALL MZDROP (IDISCD, LAUXCL(9), 'L')
                    IPNT   = KOFUCD + LAUXCL(10) + (IO -1)*(NTOT +NPAIR)
                    IQ(IPNT+1) = -1
                    GO TO 160
                  ENDIF
                  IF (IBEGCP(NPT).EQ.0) THEN
                    IBEGCP(NPT) = 1
                    CALL CDIOTY (LAUXCL(9), NOBJS(NPT),
     +              KOBJS(NDSMCP(NPT)+1), IOTYCP(NDSTCP(NPT)+1))
                  ENDIF
                ENDIF
*
*  **           Load the requested keys and object elements
*
                IPNT = KOFUCD + LAUXCL(10) + (NPL - 1) * (NTOT + NPAIR)
                DO 140 I = 1, NKEYX(NPT)
                  IND = KEYXS(NKSMCP(NPT)+I)
                  IF (IOPTCA.NE.0.AND.IND.NE.IDHINS) THEN
                    CALL CDTIMM (KEYVCK(IND), IVAL)
                    Q(IPNT+NKSTCP(NPT)+I+NPAIR) = IVAL
                  ELSE IF (IOPTCA.NE.0.AND.IOPXCA.NE.0.AND.
     +              (IND.GT.NOF1CK.AND.IND.LE.NOF1CK+2*NPARCD)) THEN
                    CALL CDTIMS (KEYVCK(IND), IVAL)
                    Q(IPNT+NKSTCP(NPT)+I+NPAIR) = IVAL
                  ELSE
                    Q(IPNT+NKSTCP(NPT)+I+NPAIR) = KEYVCK(IND)
                  ENDIF
  140           CONTINUE
                DO 145 I = 1, NOBJS(1)
                  ID     = NDSTCP(1) + I
                  II     = KOBJS(NDSMCP(1)+I)
                  IF (IOTYCP(ID).EQ.1.OR.IOTYCP(ID).EQ.2.OR.
     +                IOTYCP(ID).EQ.5)      THEN
                    IVAL   = IQ(KOFUCD+LAUXCL(9)+II)
                    Q(IPNT+ID+NPAIR) = IVAL
                  ELSE IF (IOTYCP(ID).EQ.4) THEN
                    CALL UCOPY (Q(KOFUCD+LAUXCL(9)+II), DVAL, 2)
                    Q(IPNT+ID+NPAIR) = DVAL
                  ELSE
                    Q(IPNT+ID+NPAIR) = Q(KOFUCD+LAUXCL(9)+II)
                  ENDIF
  145           CONTINUE
                IF (LAUXCL(9).NE.0) CALL MZDROP (IDISCD, LAUXCL(9), 'L')
                GO TO 160
  150         CONTINUE
*
              CALL RZCDIR (PATHY, ' ' )
              IF (IQUEST(1).NE.0) THEN
                CALL MZDROP (IDISCD, LAUXCL(10), 'L')
                IRC    = 161
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL :'//
     +          ' Illegal pathname '//PATHY//''')', IARGCD, 0)
#endif
                GO TO 999
              ENDIF
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
  155       CONTINUE
          ENDIF
          IPNT   = KOFUCD + LAUXCL(10) + (NPL - 1) * (NTOT + NPAIR)
          IQ(IPNT+1) = -1
*
  160     CALL RZCDIR (PATHY, ' ' )
          IF (IQUEST(1).NE.0) THEN
            CALL MZDROP (IDISCD, LAUXCL(10), 'L')
            IRC    = 161
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDNTPL :'//
     +      ' Illegal pathname '//PATHY//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
  170   CONTINUE
  180 CONTINUE
*
* *** Fill the N-tuples
*
      CALL HCDIR (PATHN, ' ')
      DO 190 IP = 1, NPL
        IPNT   = KOFUCD + LAUXCL(10) + (IP - 1) * (NTOT + NPAIR)
        IF (IQ(IPNT+1).GT.0) THEN
          CALL HFN (NTUP, Q(IPNT+NPAIR+1))
        ENDIF
  190 CONTINUE
*
* *** Go back and repeat if not yet completed
*
      IF (NOLD.NE.NCUR.OR.NOLDD.NE.NCURD) THEN
        NOLD   = NCUR
        NOLDD  = NCURD
        GO TO 40
      ENDIF
*                                                             END CDNTPL
  999 END
