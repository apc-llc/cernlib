*
* $Id: cdfzup.F,v 1.1.1.1 1996/02/28 16:24:11 mclareni Exp $
*
* $Log: cdfzup.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:11  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE CDFZUP (LUNFZ, CHOPT, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFZUP (LUNFZ, CHOPT, IRC*)                             *
*                                                                      *
*   Updates the data base from transcript file from unit LUNFZ         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNFZ    Logical unit number of the FZ file                      *
*     CHOPT    Character string with any of the following characters   *
*          F   Force continuation beyond errors                        *
*          O   Updating journal file is an external source             *
*          S   Updating journal file in a single record mode           *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 71 : Illegal path name in the transcript file        *
*               = 72 : Read error on the FZ file                       *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               = 78 : Illegal number of keys in journal file          *
*               = 79 : Top directory name illegal in the transcript    *
*                      file                                            *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__ONLINE))
#include "hepdb/cbsrvrn.inc"
#endif
      PARAMETER       (NLEVM=20, JBIAS=2)
      DIMENSION       NLCUR(NLEVM), KEYN(MXDMCK), IMASK(MXDMCK)
      DIMENSION       ITIME(MXPACD)
      CHARACTER       ALIAS*8, PATHN*255, PATHD*255, CHOPT*(*)
      CHARACTER       CHFOR*100, CFORM(6)*1, CHOPF*255, CHCUR(NLEVM)*1
      DATA            CFORM / 'B', 'I', 'F', 'D', 'H', 'A' /
*
*     ------------------------------------------------------------------
*
      PATHD  = ' '
      CALL UOPTC (CHOPT, 'F', IOPTF)
      CALL UOPTC (CHOPT, 'O', IOVRD)
      CALL UOPTC (CHOPT, 'S', ISNGR)
*
* *** Read the header record from the FZ file
*
   10 NWHDCF = NHEDCF
      CALL FZIN (LUNFZ, IDISCD, 0, 0, 'S', NWHDCF, IHEDCF)
      IF (IQUEST(1).GT.0)                                  GO TO 997
      IF (IQUEST(1).NE.0) THEN
         IQUEST(11) = IQUEST(1)
         IRC = 72
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZUP : FZ '
     +   //'error type '',I12)', IQUEST(11), 1)
#endif
         IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
         IRC = 0
         GO TO 10
      ENDIF
      IACT   = IHEDCF(MACTCF)
      NWKEY  = IHEDCF(MNKYCF)
      NDOP   = IHEDCF(MOPTCF)
      NWDP   = IHEDCF(MPATCF)
      NCFO   = (NWKEY + 3) / 4
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDFZUP : ' //
     +'action code '',I12)', IHEDCF(MACTCF), 1)
#endif
      IF (IACT.EQ.1) THEN
         NPNT1 = MPRECF + NWKEY + 1
         NPNT2 = NPNT1 + NDOP
      ELSE IF (IACT.EQ.2) THEN
         NPNT1 = MRECCF + 1
         NPNT2 = NPNT1 + NDOP + NCFO + 2 * NWKEY
      ELSE IF (IACT.EQ.3) THEN
         KEY7CK = IHEDCF(MINSCF)
         NPARS = IHEDCF(MPARCF)
         IF (NPARS.GT.0) THEN
            NPNT1 = MTIMCF + 2*NWKEY + NPARS
         ELSE
            NPNT1 = MTIMCF + NWKEY
         ENDIF
         NPNT2 = NPNT1 + NDOP
      ELSE IF (IACT.EQ.4) THEN
         KEY7CK = IHEDCF(MINSCF)
         NPNT1 = MINSCF + 1
         NPNT2 = NPNT1 + NDOP
      ELSE IF (IACT.EQ.5) THEN
         KEY7CK = 0
         NPNT1 = MPRECF + 2*NWKEY + 1
         NPNT2 = NPNT1
      ELSE IF (IACT.EQ.6) THEN
         KEY7CK = 0
         NPNT1 = MFLGCF + NWKEY + 1
         NPNT2 = NPNT1 + NDOP
*       NPNT2  = NPNT1
      ELSE IF (IACT.EQ.7) THEN
         KEY7CK = 0
         NPNT1 = MWDPCF + 1
         NPNT2 = NPNT1
      ELSE IF (IACT.EQ.8) THEN
         KEY7CK = IHEDCF(MDELCF)
         NPNT1 = MKEPCF + 1
         NPNT2 = NPNT1 + NDOP
      ELSE IF (IACT.EQ.9) THEN
         NPNT1 = MPRECF + NWKEY + 1
         NPNT2 = NPNT1 + NDOP
      ELSE IF (IACT.EQ.10) THEN
         NPNT1 = MPATCF + NWKEY + 1
         NPNT2 = NPNT1 + NDOP
      ELSE
         IF (LUNFZ.EQ.LUFMCD.OR.ISNGR.NE.0) GO TO 997
         GO TO 10
      ENDIF
*
      IF (IACT.GT.0.AND.IACT.LE.6.AND.IACT.NE.4) THEN
         IF (NWKEY.LT.NOF2CK.OR.NWKEY.GT.MXDMCK) THEN
            IRC = 78
            IQUEST(11) = NWKEY
            IQUEST(12) = NOF2CK
            IQUEST(13) = MXDMCK
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZUP : '
     +      //'Illegal number of keys '',I10,'' permitted range '
     +      //''',2I6)', IQUEST(11), 3)
#endif
            IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
            IRC = 0
            GO TO 10
         ENDIF
      ENDIF
*
      IF (NDOP.GT.0) THEN
         NDOP = MIN0 (NDOP, MAXLCD/4)
         CALL UHTOC (IHEDCF(NPNT1), 4, CHOPF, 4*NDOP)
         CHOPF = CHOPF(1:4*NDOP)
      ELSE
         CHOPF = ' '
      ENDIF
      CALL CDOPTS (CHOPF, IRC)
      IF (IRC.GT.0) THEN
         IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
         IRC = 0
         GO TO 10
      ENDIF
*
      CALL UHTOC (IHEDCF(NPNT2), 4, PATHN, 4*NWDP)
      PATHN  = PATHN(1:4*NWDP)
      NCH    = LENOCC (PATHN)
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__ONLINE))
      LPNUCB = 4*NWDP
      PNUPCB = PATHN
      IAUPCB = IACT
#endif
*
* *** Extract the top directory name from the pathname
*
      CALL CDTOPN (PATHN, TOPNCD, NCHRCD)
      IF (NCHRCD.EQ.0) THEN
         IRC = 79
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZUP : '
     +   //'Illegal top directory name '//PATHN//''')', IARGCD, 0)
#endif
         IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
         IRC = 0
         GO TO 10
      ENDIF
*
* *** Check if the topname matches with any of the existing topnames
*
      IPRBCA = 0
      IPRECA = 0
      LBUPCD = LTOPCD
   20 IF (LBUPCD.NE.0) THEN
         NCHR = IQ(KOFUCD+LBUPCD+MUPNCH)
         CALL UHTOC (IQ(KOFUCD+LBUPCD+MUPNAM), 4, TOP1CT, NCHR)
         IF (TOP1CT(1:NCHR).NE.TOPNCD) THEN
            LBUPCD = LQ(KOFUCD+LBUPCD)
            GO TO 20
         ELSE
            LURZCD = IQ(KOFUCD+LBUPCD+MUPLUN)
            IOUTCD = IQ(KOFUCD+LBUPCD+MUPFLG)
            IOPPCD = IQ(KOFUCD+LBUPCD+MUPSRV)
            IOPSCD = IQ(KOFUCD+LBUPCD+MUPSHR)
            NPARCD = IQ(KOFUCD+LBUPCD+MUPAIR)
            ITOPCD = IQ(KOFUCD+LBUPCD+MUPDIC)
            MXINCD = IQ(KOFUCD+LBUPCD+MUPKIN)
            LUFZCD = IQ(KOFUCD+LBUPCD+MUPJFL)
            LUBKCD = IQ(KOFUCD+LBUPCD+MUPBAK)
            IHFLCD = IQ(KOFUCD+LBUPCD+MUPHFL)
         ENDIF
*
      ELSE
         IRC = 79
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZUP : '
     +   //'Illegal top directory name '//PATHN//''')', IARGCD, 0)
#endif
         IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
         IRC = 0
         GO TO 10
      ENDIF
*
      LREFCD(1) = LQ(KOFUCD+LBUPCD-KLFZCD)
      IF (IOPBCA.EQ.0) THEN
         LUFZCF = LUFZCD
      ELSE
         LUFZCF = LUBKCD
      ENDIF
*
* *** Check if the directory is one in the list
*
      IF (LREFCD(1).NE.0) THEN
         NCUR = IQ(KOFUCD+LREFCD(1)-5)
         DO 30 I = 1, NCUR
            IPNT = KOFUCD + LREFCD(1) + MFZDIR + (I - 1) * (MXLWCD + 1)
            NCHZ = IQ(IPNT)
            CALL UHTOC (IQ(IPNT+1), 4, PAT1CT, NCHZ)
            IF ((PAT1CT(1:NCHZ).EQ.PATHN(1:NCHZ)).AND. (PATHN(NCHZ+1:
     +      NCHZ+1).EQ.'/'.OR. PATHN(NCHZ+1:NCHZ+1).EQ.' ')) GO TO 35
   30    CONTINUE
         IRC = 0
         IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
         GO TO 10
      ENDIF
*
* *** Decide overriding flag
*
   35 IOVRCD = IOVRD
      IF (LBAFCD.NE.0) THEN
         NCUR = IQ(KOFUCD+LBAFCD-5)
         DO 40 I = 1, NCUR
            IPNT = KOFUCD + LBAFCD + (I - 1) * (MXLWCD + 1) + 1
            NCHZ = IQ(IPNT)
            CALL UHTOC (IQ(IPNT+1), 4, PAT1CT, NCHZ)
            IF ((PAT1CT(1:NCHZ).EQ.PATHN(1:NCHZ)).AND.(NCHZ.EQ.NCH))
     +      THEN
               IOVRCD = 1
               GO TO 45
            ENDIF
   40    CONTINUE
      ENDIF
*
   45 CONTINUE
*
* *** call RZCDIR as the current directory is new or already updated
*
      IF (IACT.NE.2) THEN
         PAT1CT = PATHN
      ELSE
         PAT1CT = '//'//TOPNCD
      ENDIF
      CALL RZCDIR (PAT1CT, ' ')
      IF (IQUEST(1).NE.0) THEN
         IRC = 71
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZUP : '
     +   //'Illegal path name '//PAT1CT//''')', IARGCD, 0)
#endif
         IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
         IRC = 0
         GO TO 10
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      PATHD = PATHN
*
*  **   Get the IO descriptor for the header
*
      IF (LUFZCF.GT.0.AND.IACT.EQ.1) THEN
         CALL CDKEYT
         NLEV = 1
         NCUR = 5
         IFORO = 2
         CHCUR(NLEV) = CFORM(IFORO)
         DO 50 I = 1, NWKYCK
            IFORM = IOTYCK(I)
            IF (IFORM.EQ.6) IFORM = 5
            IF (IFORM.EQ.IFORO) THEN
               NCUR = NCUR + 1
            ELSE
               NLCUR(NLEV) = NCUR
               IF (NLEV.GE.NLEVM) THEN
                  IRC = 76
#if defined(CERNLIB__DEBUG)
                  IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' '
     +            //'CDFZUP : Cannot get IO descriptor '// PATHN//''')'
     +            , IARGCD, 0)
#endif
                  IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0)
     +            GO TO 998
                  IRC = 0
                  GO TO 10
               ENDIF
               NLEV = NLEV + 1
               CHCUR(NLEV) = CFORM(IFORM)
               NCUR = 1
               IFORO = IFORM
            ENDIF
   50    CONTINUE
         NLCUR(NLEV) = NCUR
*
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
         WRITE (CHFOR, 2001) (NLCUR(I), CHCUR(I), I = 1, NLEV)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
         CHFOR = ' '
         II = 1
         DO 55 I = 1, NLEV
            CALL UTWRIT (CHFOR(II:II+1), '(I2)', NLCUR(I))
            II = II + 2
            CHFOR(II:II) = CHCUR(I)
            II = II + 2
   55    CONTINUE
#endif
         II = 4 *NLEV
         CHFOR = CHFOR(1:II)//' -H'
         CALL MZIOCH (IOFMCF, NWFMCF, CHFOR(1:II+3))
      ENDIF
*
* *** Take appropriate action as directed by IACT
*
      IF (IACT.EQ.1) THEN
*
*  **   Enter data
*
         CALL CDENFZ (PATHN, LUNFZ, IRC)
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            CALL CDUPTM (IARGCD(1),IARGCD(2),IHEDCF(MPRECF+IDHINS),IRC0)
            IARGCD(3) = IRC
            CALL CDPRNT (LPRTCD, '('' CDFZUP : enter object in '//
     +      PATHN(1:60)//' time '',2I8,'' return code '',I6)', IARGCD,
     +      3)
         ENDIF
#endif
         IF (IRC.LT.0) GO TO 997
*
      ELSE IF (IACT.EQ.2) THEN
*
*  **   Create a new directory
*
         IOKYCA(IDHKSN) = 1
         KEY7CK = IHEDCF(MINSCF)
         NPNTF = NPNT1 + NDOP
         CALL UHTOC (IHEDCF(NPNTF), 4, CHFOR, NWKEY)
         CHFOR = CHFOR(1:NWKEY)
         NPNTF = NPNTF + NCFO
         DO 60 IK = 1, NWKEY
            CALL UHTOC (IHEDCF(NPNTF), 4, CTAGCK(IK), 8)
            NPNTF = NPNTF + 2
   60    CONTINUE
         CALL FZIN (LUNFZ, IDISCD, LFIXCD, JBIAS, 'A', 0, 0)
         IF (IQUEST(1).GT.0) GO TO 997
         IF (IQUEST(1).NE.0) THEN
            IQUEST(11) = IQUEST(1)
            IRC = 72
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZUP : FZ '
     +      //'error type '',I12)', IQUEST(11), 1)
#endif
            IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
            IRC = 0
            GO TO 10
         ENDIF
         IPREC = IQ(KOFUCD+LFIXCD-5)
         DELTA = Q(KOFUCD+LFIXCD+1)
         MXKP = IHEDCF(MXKPCF)
         CALL MZDROP (IDISCD, LFIXCD, 'L')
         CALL CDMKDI (PATHN, NWKEY, CHFOR, CTAGCK, MXKP, IPREC, DELTA,
     +   CHOPF, IRC)
         IOKYCA(IDHKSN) = 0
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            CALL CDUPTM (IARGCD(1), IARGCD(2), IHEDCF(MINSCF), IRC0)
            IARGCD(3) = IRC
            CALL CDPRNT (LPRTCD, '('' CDFZUP : enter new directory '//
     +      PATHN(1:60)//' time '',2I8,'' return code '',I6)', IARGCD,
     +      3)
         ENDIF
#endif
*
      ELSE IF (IACT.EQ.3) THEN
*
*  **   Delete data objects
*
         IF (NPARS.GT.0) THEN
            CALL UCOPY (IHEDCF(MTIMCF), ITIME, NPARS)
            CALL UCOPY (IHEDCF(MTIMCF+NPARS+NWKEY), KEYVCK, NWKEY)
            CALL UCOPY (IHEDCF(MTIMCF+NPARS), IMASK, NWKEY)
            CALL CDPURK (PATHN, ITIME, IMASK, KEYVCK, CHOPF, IRC)
         ELSE
            KYDAT = IHEDCF(MTIMCF+IDHKSN-1)
            KYTIM = IHEDCF(MTIMCF+IDHPTR-1)
            CALL CDPURG (PATHN, KYDAT, KYTIM, CHOPF, IRC)
         ENDIF
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            CALL CDUPTM (IARGCD(1), IARGCD(2), IHEDCF(MINSCF), IRC0)
            IARGCD(3) = IRC
            IARGCD(4) = IQUEST(2)
            CALL CDPRNT (LPRTCD, '('' CDFZUP : purge data from '//
     +      PATHN(1:60)//' time '',2I8,'' return code '',I4,I6)',
     +      IARGCD, 4)
         ENDIF
#endif
*
      ELSE IF (IACT.EQ.4) THEN
*
*  **   Delete an entire tree
*
         CALL CDDDIR (PATHN, CHOPF, IRC)
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            CALL CDUPTM (IARGCD(1), IARGCD(2), IHEDCF(MINSCF), IRC0)
            IARGCD(3) = IRC
            CALL CDPRNT (LPRTCD, '('' CDFZUP : delete the tree '//
     +      PATHN(1:60)//' time '',2I8,'' return code '',I6)', IARGCD,
     +      3)
         ENDIF
#endif
         IF (IRC.EQ.171) IRC = 0
*
      ELSE IF (IACT.EQ.5) THEN
*
*  **   Rename the keys
*
         CALL UCOPY (IHEDCF(MPRECF+1), KEYVCK, NWKEY)
         CALL UCOPY (IHEDCF(MPRECF+NWKEY+1), KEYN, NWKEY)
         CALL CDRENK (PATHN, KEYVCK, KEYN, IRC)
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            CALL CDUPTM (IARGCD(1), IARGCD(2), IHEDCF(MPRECF+IDHINS),
     +      IRC0)
            IARGCD(3) = IRC
            CALL CDPRNT (LPRTCD, '('' CDFZUP : rename keys in '//
     +      PATHN(1:60)//' time '',2I8,'' return code '',I6)', IARGCD,
     +      3)
         ENDIF
#endif
         IF (IRC.EQ.192) IRC = 0
*
      ELSE IF (IACT.EQ.6) THEN
*
*  **   Enter the name or help information
*
         IFLG = IHEDCF(MFLGCF)
         CALL UCOPY (IHEDCF(MFLGCF+1), KEYVCK, NWKEY)
         CALL CDSNAM (IFLG, KEYVCK, 0, LUNFZ, IRC)
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            IARGCD(1) = IHEDCF(MFLGCF+IDHKSN)
            CALL CDUPTM (IARGCD(2), IARGCD(3), IHEDCF(MFLGCF+IDHINS),
     +      IRC0)
            IARGCD(4) = IRC
            IF (IFLG.EQ.1) THEN
               CALL CDPRNT (LPRTCD, '('' CDFZUP : enter help '
     +         //'information for path code '',I6,'' time '',2I8,'
     +         //''' return code '',I6)', IARGCD, 4)
            ELSE
               CALL CDPRNT (LPRTCD, '('' CDFZUP : enter name '
     +         //'information for path code '',I6,'' time '',2I8,'
     +         //''' return code '',I6)', IARGCD, 4)
            ENDIF
         ENDIF
#endif
*
      ELSE IF (IACT.EQ.7) THEN
*
*  **   Rename alias name of a given directory
*
         IFLG = IHEDCF(MFLGCF)
         NWDF = IHEDCF(MWDPCF)
         ALIAS = ' '
         CALL UHTOC (IHEDCF(MWDPCF+NWDP+1), 4, ALIAS, 8)
         CALL UHTOC (IHEDCF(MWDPCF+NWDP+3), 4, CHOPF, 4*NWDF)
         CHOPF = CHOPF(1:4*NWDF)
         CALL CDEALI (CHOPF, ALIAS, IFLG, IRC)
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            CALL CDPRNT (LPRTCD, '('' CDFZUP : enter alias name for '
     +      //'path '//CHOPF(1:60)//' return code '',I6)', IRC, 1)
         ENDIF
#endif
         IRC = 0
*
      ELSE IF (IACT.EQ.8) THEN
*
*  **   Delete a few partitions in a partitioned directory
*
         IKEEP = IHEDCF(MKEPCF)
         CALL CDPURP (PATHN, IKEEP, CHOPF, IRC)
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            IARGCD(1) = IKEEP
            IARGCD(2) = IRC
            CALL CDPRNT (LPRTCD, '('' CDFZUP : delete all but '',I6,'' '
     +      // 'partitions in '//PATHN(1:60)//' return code '',I6)',
     +      IARGCD, 2)
         ENDIF
#endif
         IRC = 0
*
      ELSE IF (IACT.EQ.9) THEN
*
*  **   Use the forbidden path for updating data base (replace object)
*
         CALL FZIN (LUNFZ, IDISCD, LFIXCD, 2, 'A', 0, 0)
         IF (IQUEST(1).GT.0) GO TO 997
         IF (IQUEST(1).NE.0) THEN
            IQUEST(11) = IQUEST(1)
            IRC = 72
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZUP : FZ '
     +      //'error type '',I12)', IQUEST(11), 1)
#endif
            GO TO 991
         ENDIF
         CALL UCOPY (IHEDCF(MPRECF+1), KEYN, NWKEY)
         CALL CDDONT (PATHN, IDISCD, LFIXCD, KEYN, CHOPF, IRC)
         CALL MZDROP (IDISCD, LFIXCD, 'L')
         LFIXCD = 0
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            IARGCD(1) = IRC
            CALL CDPRNT (LPRTCD, '('' CDFZUP : updated data base in '//
     +      'replace mode for '//PATHN(1:60)//' return code '',I6)',
     +      IARGCD, 1)
         ENDIF
#endif
*
      ELSE IF (IACT.EQ.10) THEN
*
*  **   Recreate the dictionary record
*
         CALL CDRDIC (TOPNCD, IRC)
#if defined(CERNLIB__DEBUG)
         IF (IDEBCD.GT.0) THEN
            IARGCD(1) = IRC
            CALL CDPRNT (LPRTCD, '('' CDFZUP : recreates dictionary d'//
     +      'irectory for '//TOPNCD//' return code '',I6)', IARGCD, 1)
         ENDIF
#endif
*
      ENDIF
  991 IF (IRC.GT.0) THEN
         IF (LUNFZ.EQ.LUFMCD.OR.IOPTF.EQ.0.OR.ISNGR.NE.0) GO TO 998
         IRC = 0
         GO TO 10
      ENDIF
      IF (LUNFZ.NE.LUFMCD.AND.ISNGR.EQ.0)                  GO TO 10
*
  997 IRC    = 0
  998 CONTINUE
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I2,A1,1X))
#endif
*                                                             END CDFZUP
      END
#endif
