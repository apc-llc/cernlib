*
* $Id: cdentb.F,v 1.1.1.1 1996/02/28 16:24:33 mclareni Exp $
*
* $Log: cdentb.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDENTB (PATHN, LSUP,IUDIV, NWDIM,NOBJ, KEY, CHOPT, IRC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDENTB (PATHN, LSUP,IUDIV, NWDIM,NOBJ,KEY, CHOPT, IRC*) *
*                      CHOPT)                                          *
*                                                                      *
*   Stores data from memory to disk for a number of objects in a go.   *
*   It is useful in a Batch operation to save real time spent          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LSUP     Vector containing the addresses of the banks where      *
*              data reside                                             *
*     IUDIV    Division index where the data reside                    *
*     NWDIM    First dimension of the array KEY                        *
*     NOBJ     Number of objects to be inserted                        *
*     KEY      Two dimensional array with the first dimension NWDIM,   *
*              specifying the key elements for each object and the     *
*              secod dimension NOBJ, specifying the number of objects  *
*              (Keys 3,4,5 and 8 onwards to be filled in by user on    *
*              input; the DB system keys will be filled in here at the *
*              time of output)                                         *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          D   Store only the differences from an existing object      *
*          F   Updates with a fully matched data object (in user keys) *
*          K   Store data only inside the keys (not yet installed)     *
*          H   Insertion time as supplied by user to be honoured       *
*          P   Store data compressed (bit packing)                     *
*          T   Special text type of data (to be used with R)           *
*          Y   Store with full RZ option (No compression to be made)   *
*          Z   Store only nonzero elements. An element is considered to*
*              to be zero if its absolute value is less than DELTA     *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user,   CDSTOM                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 61 : Illegal number of keys (NWDIM < NWKEY)          *
*               = 71 : Illegal path name                               *
*               = 73 : RZOUT fails to write on disk                    *
*               = 74 : Error in RZRENK in updating key values for      *
*                      partitioned data set                            *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               = 77 : FZOUT fails to write on to the sequential file  *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/czpack.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (NLEVM=20)
      INTEGER         NLCUR(NLEVM)
      DIMENSION       KEY(NWDIM,2), LSUP(9)
      CHARACTER       PATHY*80, PATHX*16, CHFOR*100, CFORM(6)*1
      CHARACTER       CHCUR(NLEVM)*1, CHOP*1
      CHARACTER       PATHN*(*), CHOPT*(*), CHOP0*80, PATHL*80
      DATA            CFORM /'B', 'I', 'F', 'D', 'H', 'A'/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
#include "zebra/q_sbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                GO TO 999
*
      IF (IOPFCA.NE.0) IOPDCA = 1
      IF (IOPTCA.NE.0) IOPYCA = 1
      IF (IOPYCA.NE.0) THEN
        IOPPCA = 0
        IOPZCA = 0
      ENDIF
      IF (IOPPCA.NE.0) IOPZCA = 0
*
* *** Load top directory information; gets in PAT1CT complete path name
*
      CALL CDLDUP (PATHN, 1, IRC)
      IF (IRC.NE.0)                GO TO 999
*
* *** Set the current directory path name
*
      PATHL  = ' '
      PATHY  = PAT1CT
      PATHX  = ' '
      CALL RZCDIR (PATHY, ' ')
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      KST    = NWKYCK + 1
      CALL CDKYTG
      IF (NKEYCK.NE.0)  THEN
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ELSE
        IOPTP  = 0
      ENDIF
*
* *** Load IPREC/DELTA from dictionary; choose the transcript file
*
      CALL CDLDIC (PATHY, 1, IRC)
      IF (IRC.NE.0)                GO TO 999
      IF (ICMPCD.EQ.0) THEN
        IOPYCA = 1
        PACKCZ = .FALSE.
      ELSE IF (ICMPCD.EQ.2.AND.IOPZCA.NE.0) THEN
        PACKCZ = .TRUE.
        PRECCZ = DELTCD
      ELSE
        PACKCZ = .FALSE.
      ENDIF
      IF (IOPBCA.EQ.0) THEN
        LUFZCF = LUFZCD
      ELSE
        LUFZCF = LUBKCD
      ENDIF
*
* *** Check the number of keys
*
      IF (NWDIM.LT.NWKYCK)  THEN
        IRC        = 61
        IQUEST(11) = NWKYCK
        IQUEST(12) = NWDIM
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENTB : Too man'//
     +     'y keys '',I6,'' maximum permitted '',I6)', IQUEST(11), 2)
#endif
        GO TO 999
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
* *** Setup the server
*
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        LUFZCF = LUFMCD
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (TOPNCD, 0, IRC)
        IF (IRC.NE.0)              GO TO 999
#endif
#if defined(CERNLIB__P3CHILD)
        LUFZCF = LODBP3
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      ENDIF
#endif
*
* *** Get the IO descriptor for the header
*
      IF (LUFZCF.GT.0) THEN
        NDOPC  = LENOCC (CHOPT)
        IF (INDEX (CHOPT, 'H') .EQ. 0) THEN
          CHOP0  = 'H'//CHOPT
          NDOPC  = NDOPC + 1
        ELSE
          CHOP0  = CHOPT
        ENDIF
        NDOP   = (NDOPC + 3) / 4
        NLEV   = 1
        NCUR   = 5
        IFORO  = 2
        CHCUR(NLEV) = CFORM(IFORO)
        IF (PACKCZ.AND.IOPUCA.EQ.0) THEN
          NLCUR(NLEV) = 4
          IFORO  = 3
          NCUR   = 1
          NLEV   = NLEV + 1
          CHCUR(NLEV) = CFORM(IFORO)
          CALL UCOPY (PRECCZ, IHEDCF(MPRECF), 1)
        ELSE
          IHEDCF(MPRECF) = IPRECD
        ENDIF
        DO 25 I = 1, NWKYCK
          IFORM  = IOTYCK(I)
          IF (IFORM.EQ.6) IFORM = 5
          IF (IFORM.EQ.IFORO) THEN
            NCUR   = NCUR + 1
          ELSE
            NLCUR(NLEV) = NCUR
            IF (NLEV.GE.NLEVM) THEN
              IRC    = 76
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENTB : '//
     +        'Cannot get IO descriptor '//PATHY//''')', IARGCD, 0)
#endif
              GO TO 999
            ENDIF
            NLEV   = NLEV + 1
            CHCUR(NLEV) = CFORM(IFORM)
            NCUR   = 1
            IFORO  = IFORM
          ENDIF
   25   CONTINUE
        NLCUR(NLEV) = NCUR
*
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
        WRITE (CHFOR, 2001) (NLCUR(I), CHCUR(I), I = 1, NLEV)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
        CHFOR  = ' '
        II     = 1
        DO 26 I = 1, NLEV
          CALL UTWRIT (CHFOR(II:II+1), '(I2)', NLCUR(I))
          II     = II + 2
          CHFOR(II:II) = CHCUR(I)
          II     = II + 2
   26   CONTINUE
#endif
        II     = 4 *NLEV
        CHFOR = CHFOR(1:II)//' -H'
        CALL MZIOCH (IOFMCF, NWFMCF, CHFOR(1:II+3))
*
*  **   Partially fill up the header
*
        NCHR   = LENOCC (PATHY)
        NWDP   = (NCHR + 3) / 4
        NWDH   = NWDP + NDOP + NWKYCK + 5
        IHEDCF(MACTCF) = 1
        IHEDCF(MNKYCF) = NWKYCK
        IHEDCF(MOPTCF) = NDOP
        IHEDCF(MPRECF) = NWDP
        IF (NDOP.GT.0)
     +    CALL UCTOH (CHOP0, IHEDCF(NWKYCK+MPRECF+1), 4, 4*NDOP)
        CALL UCTOH (PATHY, IHEDCF(NWKYCK+NDOP+MPRECF+1), 4, 4*NWDP)
      ENDIF
*
* *** Take necessary action for partitioned and nonpartiitined datasets
*
      IF (IOPTP.EQ.0) THEN
        KOBJ   = 0
      ELSE
        KPNT   = IUHUNT (NKEYCK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                   NKEYCK*KST, KST)
        IF (KPNT.NE.0) THEN
          NK     = (KPNT - MPSRCD) / KST + 1
        ELSE
          NK     = NKEYCK
        ENDIF
        CALL CDKEYR (NK, NWKYCK, KYP1CK)
        KOBJ   = KYP1CK(MOBJCD)
        MXKP   = KYP1CK(MXKPCD)
        NWKYS  = NWKYCK
        CHFOR = ' '
        DO 30 I = 1, NWKYS
          IF (I.EQ.1) THEN
            CHFOR  = CFORM(IOTYCK(I))
          ELSE
            CHFOR  = CHFOR(1:I-1)//CFORM(IOTYCK(I))
          ENDIF
   30   CONTINUE
*
        CALL CDPATH (PATHX, NKEYCK)
        CALL RZCDIR (PATHX, ' ')
        IF (IQUEST(1).NE.0)        GO TO 991
        NKEYCK = IQUEST(7)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
*
*  **   Make a different subdirectory if there are too many keys
*
        IF (NKEYCK.GE.MXKP) THEN
          IF (IOPHCA.EQ.0.OR.KEY(IDHINS,1).LE.0) THEN
            KEY7CK = 0
          ELSE
            KEY7CK = KEY(IDHINS,1)
          ENDIF
          IF (ICMPCD.EQ.2) THEN
            CHOP0  = 'ZP'
          ELSE IF (ICMPCD.NE.0) THEN
            CHOP0 = 'CP'
          ELSE
            CHOP0 = 'P '
          ENDIF
          CALL CDMKDI (PATHY, NWKYS, CHFOR, CTAGCK, MXKP, IPRECD,
     +                 DELTCD, CHOP0, IRC)
          IF (IRC.NE.0)            GO TO 999
          CALL RZCDIR (PATHY, ' ')
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          KPNT   = IUHUNT (NKEYCK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYCK*KST, KST)
          IF (KPNT.NE.0) THEN
            NK     = (KPNT - MPSRCD) / KST + 1
          ELSE
            NK     = NKEYCK
          ENDIF
          CALL CDKEYR (NK, NWKYCK, KYP1CK)
          KOBJ   = KYP1CK(MOBJCD)
          CALL CDPATH (PATHX, NKEYCK)
          CALL RZCDIR (PATHX, ' ')
          IF (IQUEST(1).NE.0)      GO TO 991
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
        ENDIF
        CALL UCOPY (KYP1CK, KYP2CK, NWKYCK)
*
      ENDIF
*
* *** Get the Serial number of the last object inserted
*
      LOBJ   = KOBJ
      IF (NKEYCK.GT.0) THEN
        ISTP   = NWKYCK + 1
        DO 35 IK = 1, NKEYCK
          IP     = KOFSCD + LCDRCD + IKDRCD + (IK-1)*ISTP + IDHKSN
          IF (IQ(IP).GT.LOBJ) LOBJ = IQ(IP)
   35   CONTINUE
      ENDIF
      NINS   = NKEYCK
*
* *** Lock the directory if necessary
*
      IF (IOPPCD.EQ.0.AND.IOPSCD.NE.0) THEN
        CALL RZCDIR (PATHY, ' ')
        NKEYCK = IQUEST(7)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        CALL RZLOCK ('CDENTB')
        PATHL  = PATHY
        IF (IOPTP.NE.0) THEN
          CALL RZCDIR (PATHX, ' ')
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
        ENDIF
      ENDIF
*
* *** Loop over all the objects
*
      DO 50 IOBJ = 1, NOBJ
        LOBJ   = LOBJ + 1
        NINS   = NINS + 1
*
        IDB    = ICDTYP (LSUP(IOBJ))
        IF (IDB.EQ.2.OR.IDB.EQ.3) THEN
          IOPTR  = 0
        ELSE
          IOPTR  = 1
        ENDIF
*
        IF (IOPYCA.NE.0 .OR. IOPTR.NE.0 .OR. IOPTCA.NE.0)  THEN
          IF (IOPTCA.NE.0) THEN
            CHOP   = 'S'
          ELSE IF (IOPYCA.NE.0) THEN
            CHOP   = 'L'
          ELSE
            CHOP   = ' '
          ENDIF
        ELSE
          CHOP   = 'S'
        ENDIF
*
*  **   Fill up Key vectors 1,2,6,7
*
        KEY(IDHKSN,IOBJ) = LOBJ
        KEY(IDHPTR,IOBJ) = 0
        KEY(IDHFLG,IOBJ) = MSBIT0 (KEY(IDHFLG,IOBJ), JRZUCD)
        KEY(IDHFLG,IOBJ) = MSBIT0 (KEY(IDHFLG,IOBJ), JPRTCD)
        KEY(IDHFLG,IOBJ) = MSBIT0 (KEY(IDHFLG,IOBJ), JASFCD)
        KEY(IDHFLG,IOBJ) = MSBIT0 (KEY(IDHFLG,IOBJ), JIGNCD)
        IF (IOPHCA.EQ.0.OR.KEY(IDHINS,IOBJ).LE.0) THEN
          CALL DATIME (IDATE, ITIME)
          CALL CDPKTM (IDATE, ITIME, IDATM, IRC)
          KEY(IDHINS,IOBJ) = IDATM
        ENDIF
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
*
        IF (IOPPCD.NE.0) THEN
          CALL CDWLOK (IRC)
          IF (IRC.NE.0)            GO TO 999
        ENDIF
#endif
*
*  **   Write the sequential output if needed
*
        IF (LUFZCF.GT.0) THEN
          CALL UCOPY (KEY(1,IOBJ), IHEDCF(MPRECF+1), NWKYCK)
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
          IF (IOPPCD.NE.0) IHEDCF(MPRECF+IDHKSN) = 0
#endif
#if defined(CERNLIB__P3CHILD)
          RNDBP3 = 'CDENTB '
          NWDBP3 = 2
          CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
          CALL CDCHLD
          IRC    = IQDBP3
          IF (IRC.NE.0)            GO TO 997
#endif
          CALL FZOUT (LUFZCF, IUDIV, LSUP(IOBJ), 1, CHOP, IOFMCF,
     +                NWDH, IHEDCF)
          IF (IQUEST(1).NE.0) THEN
            IRC        = 77
            IQUEST(11) = IOBJ
            IQUEST(12) = NOBJ
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENTB : Err'//
     +      'or in FZOUT while writing Data for '',2I12)', IQUEST(11),2)
#endif
            GO TO 997
          ENDIF
        ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
        IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
          CALL CDCWSV (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
          GO TO 50
        ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
*
        IF (IOPYCA.NE.0 .OR. IOPTR.NE.0 .OR. IOPTCA.NE.0)  THEN
*
*  **     RZ mode output
*
          KEY(IDHFLG,IOBJ) = MSBIT1 (KEY(IDHFLG,IOBJ), JRZUCD)
          IF (IOPTCA.NE.0)
     +      KEY(IDHFLG,IOBJ) = MSBIT1 (KEY(IDHFLG,IOBJ), JASFCD)
*
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
          CALL RZOUT (IUDIV, LSUP(IOBJ), KEY(1,IOBJ), ICYCLE, CHOP)
*
        ELSE
*
*  **     Copy data to DB internal store
*
*  **     0 Data word : do not pack
*
          IF (IQ(KOFUCD+LSUP(IOBJ)-1).EQ.0) THEN
            IRSET  = 1
            IOPPS  = IOPPCA
            IOPZS  = IOPZCA
            IOPPCA = 0
            IOPZCA = 0
          ELSE
            IRSET  = 0
          ENDIF
          CALL CDFRUS (LSUP(IOBJ), LSTRCL(1), IPRECD, IRC)
          IF (IRC.NE.0) THEN
            IF (IRSET.NE.0) THEN
              IOPPCA = IOPPS
              IOPZCA = IOPZS
            ENDIF
            GO TO 997
          ENDIF
*
*  **     Compress the data if requested
*
          IF (IOPPCA.EQ.0.AND.IOPZCA.EQ.0) THEN
            LREFCL(1) = LSTRCL(1)
            IF (IRSET.NE.0) THEN
              IOPPCA = IOPPS
              IOPZCA = IOPZS
            ENDIF
          ELSE
            NOLD   = NKEYCK
            NKEYCK = NINS - 1
            CALL CDCOMP (LSTRCL(1), LREFCL(1), KEY(1,IOBJ), IRC)
            NKEYCK = NOLD
          ENDIF
          IF (IRC.NE.0)            GO TO 997
*
*  **     Drop the uncompressed data
*
          IF (LREFCL(1).NE.LSTRCL(1)) CALL MZDROP (IDISCD,LSTRCL(1),'L')
*
*  **     Write on to disk
*
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
          CALL RZOUT (IDISCD, LREFCL(1), KEY(1,IOBJ), ICYCLE, 'S')
          IRC    = IQUEST(1)
          CALL MZDROP (IDISCD, LREFCL(1), 'L')
          IQUEST(1) = IRC
          IF (IRC.EQ.77)           GO TO 997
*
        ENDIF
*
        IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
        NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
        IF (IOPTP.NE.0) THEN
          CALL CDPVAL (KEY(1,IOBJ))
        ENDIF
        IF (IQUEST(1).NE.0)        GO TO 993
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.1) THEN
          CALL UCOPY (KEY(1,IOBJ), KEYNCK, NWKYCK)
          IARGCD(1) = IDATE
          IARGCD(2) = ITIME
          CALL CDPRNT (LPRTCD, '(/,'' CDENTB : Data was inserted into'//
     +         '   '//PATHY//''',/,10X,''on the '',I8,'' at '',I6,'' '//
     +         'with Key-Vector '')', IARGCD, 2)
          CALL CDKEYT
          CALL CDPRKY (NWKYCK, KEYNCK, IOTYCK, IRC)
        ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
        IF (IOPTP.NE.0) THEN
          IF (NINS.GE.MXKP.AND.IOBJ.LT.NOBJ) THEN
            CALL RZCDIR (PATHY, ' ')
            IF (IQUEST(1).NE.0)    GO TO 991
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            NKEYCK = IQUEST(7)
*
*  **       Rename Keys 3 and 4 of the latest subdirectory
*
            CALL RZRENK (KYP1CK, KYP2CK)
            IF (IQUEST(1).NE.0) THEN
              IRC    = 74
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) THEN
                CALL UCOPY  (KYP1CK, IARGCD(1),        NSYSCK)
                CALL UCOPY  (KYP2CK, IARGCD(NSYSCK+1), NSYSCK)
                CALL CDPRNT (LPRTCD, '(/,'' CDENTB : Error in RZREN'//
     +               'K while writing data for '//PATHY//''',/(10X,'//
     +               '7I12))', IARGCD, 2*NSYSCK)
              ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
              GO TO 998
            ENDIF
*
*  **       Make a different subdirectory if there are too many keys
*
            IF (IOPHCA.EQ.0.OR.KEY(IDHINS,IOBJ+1).LE.0) THEN
              KEY7CK = 0
            ELSE
              KEY7CK = KEY(IDHINS,IOBJ+1)
            ENDIF
            IF (ICMPCD.EQ.2) THEN
              CHOP0  = 'ZP'
            ELSE IF (ICMPCD.NE.0) THEN
              CHOP0 = 'CP'
            ELSE
              CHOP0 = 'P '
            ENDIF
            CALL CDMKDI (PATHY, NWKYS, CHFOR, CTAGCK, MXKP, IPRECD,
     +                   DELTCD, CHOP0, IRC)
            IF (IRC.NE.0)          GO TO 998
            CALL RZCDIR (PATHY, ' ')
            NKEYCK = IQUEST(7)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            KPNT   = IUHUNT (NKEYCK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                       NKEYCK*KST, KST)
            IF (KPNT.NE.0) THEN
              NK     = (KPNT - MPSRCD) / KST + 1
            ELSE
              NK     = NKEYCK
            ENDIF
            CALL CDKEYR (NK, NWKYCK, KYP1CK)
            CALL CDPATH (PATHX, NKEYCK)
            CALL RZCDIR (PATHX, ' ')
            IF (IQUEST(1).NE.0)    GO TO 991
            NKEYCK = IQUEST(7)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            NINS   = 0
            CALL UCOPY (KYP1CK, KYP2CK, NWKYCK)
          ENDIF
        ENDIF
#endif
   50 CONTINUE
      GO TO 997
*
* *** Error messages
*
  991 IRC    = 71
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENTB : Illegal '//
     +   'Path Name '//PATHY//PATHX(1:8)//''')', IARGCD, 0)
#endif
      GO TO 998
#if !defined(CERNLIB__P3CHILD)
*
  993 IRC    = 73
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
      IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENTB : Error in '//
     +   'RZOUT while writing Data for '//PATHY//PATHX(1:8)//''')',
     +   IARGCD, 0)
#endif
*
  997 CONTINUE
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      IF (IOPPCD.NE.0)             GO TO 999
#endif
#if !defined(CERNLIB__P3CHILD)
      IF (IOPTP.NE.0)  THEN
        IF (NINS.GT.0)  THEN
          CALL RZCDIR (PATHY, ' ')
          IF (IQUEST(1).NE.0) THEN
            IF (IRC.EQ.0)          GO TO 991
          ELSE
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            NKEYCK = IQUEST(7)
*
*  **       Rename Keys 3 and 4 of the latest subdirectory
*
            IF (IRC.EQ.0) THEN
              CALL RZRENK (KYP1CK, KYP2CK)
              IF (IQUEST(1).NE.0) THEN
                IRC    = 74
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
                IF (IDEBCD.GT.0) THEN
                  CALL UCOPY  (KYP1CK, IARGCD(1),        NSYSCK)
                  CALL UCOPY  (KYP2CK, IARGCD(NSYSCK+1), NSYSCK)
                  CALL CDPRNT (LPRTCD, '(/,'' CDENTB : Error in RZREN'//
     +                 'K while writing data for '//PATHY//''',/(10X,'//
     +                 '7I12))', IARGCD, 2*NSYSCK)
                ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
*
*  ** Free the locked directory if any
*
#endif
  998 CONTINUE
#if !defined(CERNLIB__P3CHILD)
      IF (PATHL.NE.' ') THEN
        CALL RZCDIR (PATHL, ' ')
        NKEYCK = IQUEST(7)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        CALL RZFREE ('CDENTB')
      ENDIF
#endif
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I2,A1,1X))
#endif
*                                                             END CDENTB
  999 END
