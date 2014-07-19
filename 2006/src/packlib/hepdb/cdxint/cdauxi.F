*
* $Id: cdauxi.F,v 1.1.1.1 1996/02/28 16:24:46 mclareni Exp $
*
* $Log: cdauxi.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDAUXI
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDAUXI                                                  *
*                                                                      *
*   Action Routines for menu /HEPDB/AUXILIARY                          *
*                                                                      *
*   Allowed Actions :                                                  *
*                                                                      *
*     CDASCI, CDDISP, CDEALI, CDEDIT, CDEHLP, CDENAM, CDEXTR, CDKEEP,  *
*     CDNTPL, CDPEEK, CDPTIM, CDPURG, CDPURK, CDPURP, CDRALI, CDRDIC,  *
*     CDRENK, CDRHLP, CDRNAM, CDRTFZ, CDSHOW, CDSUMY, CDUTIM, CDVIEW,  *
*     CDWRITE                                                          *
*                                                                      *
*   Called by KUIP routine                                             *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cplots.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/cxlink.inc"
      PARAMETER       (NOBJM=100)
      DIMENSION       NKEYX(NPMXCP), NOBJS(NPMXCP)
      DIMENSION       KEYXS(NOBJM), KOBJS(NOBJM), ISEL(20)
      CHARACTER       CPATL*32, CHOPT*32, PATHN*80, CHPRO*32, CTEMP*5
      CHARACTER       YESNO*4, CFNAM*80, PATHS(NPMXCP)*80
      CHARACTER       CRZPA*80, CTITL*80, CTAG(NOBJM)*8, CHOP*32
      CHARACTER       ALIAS*8, CHTAG*8, CFMT*120
      DATA            PATHN /' '/, ALIAS /' '/, CTITL /' '/
      DATA            NWDS /0/, NPATH /1/, IDN /0/, KYI /0/, KYEL /0/
      DATA            LUNFZ /0/, IDATM /0/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      CALL KUPATL (CPATL, NPAR)
*
      IF (CPATL.EQ.'CDASCI') THEN
*
*  **   CDASCI
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CFNAM, NCH)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL KUGETC (CHOPT, NCH)
        CALL UOPTC  (CHOPT, 'X', IOPXCA)
        CALL CDEDAS (CFNAM, NCH, PATHN, IRC)
*
      ELSE IF (CPATL.EQ.'CDDISP') THEN
*
*  **   CDDISP
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CFNAM, NCF)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL CDOPFL (LUKYCX, CFNAM, 'UNKNOWN', ISTAT)
        IF (ISTAT.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : error '',I12,'' in ope'//
     +         'ning file '//CFNAM(1:NCF)//''')', ISTAT, 1)
          GO TO 999
        ENDIF
        CALL KUGETC (CHOPT, NCH)
        CALL UOPTC (CHOPT, 'H', IOPHCC)
        CALL UOPTC (CHOPT, 'X', IOPXCA)
        CHOP   = 'K'//CHOPT
        CALL CDDISP (LUKYCX, PATHN, CHOP, IRC)
        CALL CDCLFL (LUKYCX)
        CALL KUEDIT (CFNAM, IST)
*
      ELSE IF (CPATL.EQ.'CDEALI') THEN
*
*  **   CDEALI
*
        CALL KUGETC (PATHN, NCT)
        CALL KUGETC (ALIAS, NCH)
        CALL KUGETI (IFLAG)
        CALL CDEALI (PATHN, ALIAS, IFLAG, IRC)
        IF (IRC.EQ.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Alias name of '//
     +         PATHN(1:NCT)//' is '//ALIAS//''')', IARGCD, 0)
        ELSE
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Error in entering alia'//
     +         's name for '//PATHN(1:NCT)//''')', IARGCD, 0)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDEDIT') THEN
*
*  **   CDEDIT
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CFNAM, NCF)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL KUGETC (CHOPT, NCH)
        CALL UOPTC  (CHOPT, 'X', IOPXCA)
        CALL KUPROC ('Create Directory ? (Y/N)', YESNO, NCH)
        IF (YESNO(1:1).EQ.'Y') THEN
          CALL KUPROI ('Number of user keys ', NKEX)
          CALL KUPROC ('Character option    ', CHOPT, NCP)
          CALL CDCRDR (PATHN, NKEX, CHOPT, IRC)
          IF (IRC.NE.0) GO TO 999
        ENDIF
        CALL KUPROC ('Keys Insert/Update ? (Y/N)', YESNO, NCH)
        IF (YESNO(1:1).EQ.'Y') THEN
          CALL KUPROC ('Horizontal or Vertical Mode ? (H/V)',
     +                  YESNO, NCH)
          CALL UOPTC (YESNO, 'H', IOPHCC)
          CALL CDEDKY (CFNAM, NCF, PATHN, 'S', IRC)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDEHLP') THEN
*
*  **   CDEHLP
*
        CALL KUGETC (PATHN, NCT)
        CALL KUGETC (CFNAM, NCH)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL KUEDIT (CFNAM, IST)
        IF (IST.EQ.0) THEN
          CALL CDOPFL (LUKYCX, CFNAM, 'UNKNOWN', ISTAT)
          IF (ISTAT.EQ.0) THEN
            CALL CDEHLP (PATHN, LUKYCX, IRC)
            CALL CDCLFL (LUKYCX)
            IF (IRC.EQ.0) THEN
              CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Help info. for '//
     +             PATHN(1:NCT)//' is stored'')', IARGCD, 0)
            ELSE
              CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : error '',I6,'' in '//
     +             'storing Help info. for '//PATHN(1:NCT)//''')',
     +             IRC, 1)
            ENDIF
          ELSE
            CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : error '',I12,'' in o'//
     +           'pening file '//CFNAM(1:NCH)//''')', ISTAT, 1)
          ENDIF
        ELSE
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : error '',I12,'' in ed'//
     +         'iting file '//CFNAM(1:NCH)//''')', IST, 1)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDENAM') THEN
*
*  **   CDENAM
*
        CALL KUGETC (PATHN, NCT)
        CALL KUGETI (NWDS)
        IF (NWDS.GT.NOBJM) THEN
          IARGCD(1) = NWDS
          IARGCD(2) = NOBJM
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Too many data elements'//
     +         ' '',I12,'' maximum '',I5,'' is allowed'')', IARGCD, 2)
          GO TO 999
        ENDIF
        IF (NWDS.GT.0) THEN
          DO 5 I = 1, NWDS
            WRITE (CHPRO, 1001) I
            CALL CDPROC (CHPRO(1:10), CHTAG, NCH)
            CTAG(I) = CHTAG
    5     CONTINUE
          CALL CDENAM (PATHN, NWDS, CTAG, IRC)
          IF (IRC.EQ.0) THEN
            CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Tags of data for '//
     +           PATHN(1:NCT)//' is stored'')', IARGCD, 0)
          ELSE
            CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : error '',I6,'' in st'//
     +           'oring Tags of data for '//PATHN(1:NCT)//''')', IRC, 1)
          ENDIF
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDKEEP') THEN
*
*  **   CDKEEP
*
        CALL KUGETI (NPATH)
        CALL KUGETC (CHOPT, NCH)
        IF (NPATH.GT.0) THEN
          DO 10 IP = 1, NPATH
            WRITE (CHPRO, 1002) IP
            CALL KUPROC (CHPRO(1:15), PATHS(IP), NCH)
   10     CONTINUE
          CALL CDKEEP (PATHS, NPATH, CHOPT, IRC)
          IARGCD(1) = NPATH
          IARGCD(2) = IRC
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : All but '',I6,'' direc'//
     +         'tory trees deleted - return code '',I6)', IARGCD, 2)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDNTPL') THEN
*
*  **   CDNTPL
*
        CALL KUGETI (NPATH)
        IF (NPATH.LT.1) THEN
          NPATH = 1
        ELSE IF (NPATH.GT.NPMXCP) THEN
          NPATH = NPMXCP
        ENDIF
        CALL KUGETI (NMASK)
        CALL KUGETC (CHOPT, NCH)
        CALL KUGETI (IDN)
        CALL KUGETC (CTITL, NCT)
        CALL KUGETC (CRZPA, NCR)
        CALL KUGETI (NPRIM)
        CALL HCDIR  (PATHN, 'R')
        CALL HCDIR  (CRZPA, ' ')
        IF (IQUEST(1).NE.0) THEN
          CALL CDPRNT (L3PRCX, '('' RZ-path '//CRZPA(1:NCR)//' for '//
     +         'N-tuple is illegal'')', IARGCD, 0)
          GO TO 999
        ENDIF
        CALL HCDIR (CRZPA, 'R')
        CALL CDOPTS (CHOPT, IRC)
        IF (IRC.NE.0)                     GO TO 999
        CALL VZERO (KEYSCX, 100)
        CALL VZERO (MASKCX, 100)
        CALL CDRVNT (NPATH, PATHS, NMASK, MASKCX, KEYSCX, NVAR, NKST,
     +               NDST, NKEYX, KEYXS, NOBJS, KOBJS, CTAG, NOBJM, IRC)
        IF (IRC.NE.0)                     GO TO 999
        IF (NVAR.LT.1) THEN
          CALL CDPRNT (L3PRCX, '('' Too few  variables for N-tuple'')',
     +         IARGCD, 0)
          GO TO 999
        ENDIF
*  **    Book the N-tuple
        CALL HCDIR  (PATHN, ' ')
        CALL HBOOKN (IDN, CTITL(1:NCT), NVAR, CRZPA, NPRIM, CTAG)
        CALL CDNTPL (IDN, PATHS, NPATH, NOBJS, KOBJS, NKEYX, KEYXS,
     +               MASKCX, KEYSCX, CHOPT, IRC)
        IF (IRC.NE.0) CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : error'','//
     +  'I12,'' in routine CDNTPL'')', IRC, 1)
        CALL HCDIR  (PATHN, ' ')
*
      ELSE IF (CPATL.EQ.'CDPEEK') THEN
*
*  **   CDPEEK
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CFNAM, NCF)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL CDOPFL (LUKYCX, CFNAM, 'UNKNOWN', ISTAT)
        IF (ISTAT.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : error '',I12,'' in ope'//
     +         'ning file '//CFNAM(1:NCF)//''')', ISTAT, 1)
          GO TO 999
        ENDIF
        CALL KUGETC (CHOPT, NCH)
        CALL UOPTC (CHOPT, 'H', IOPHCC)
        CALL UOPTC (CHOPT, 'X', IOPXCA)
        CALL CDPEEK (CFNAM, NCF, PATHN, IRC)
*
      ELSE IF (CPATL.EQ.'CDPTIM') THEN
*
*  **   CDPTIM
*
        CALL KUGETI (IDATE)
        CALL KUGETI (ITIME)
        CALL KUGETC (CHOPT, NCH)
        IOPTM  = INDEX (CHOPT, 'M')
        IF (IOPTM.EQ.0) THEN
          CALL CDPKTS (IDATE, ITIME, IARGCD(1), IRC)
        ELSE
          CALL CDPKTM (IDATE, ITIME, IARGCD(1), IRC)
        ENDIF
        IARGCD(2) = IDATE
        IARGCD(3) = ITIME
        CALL CDPRNT (L3PRCX, '(/,2X,I10,'' is the packed integer for '//
     +       'date and time : '',2I10,/)', IARGCD, 3)
*
      ELSE IF (CPATL.EQ.'CDPURG') THEN
*
*  **   CDPURG
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETI (KYDAT)
        CALL KUGETI (KYTIM)
        CALL KUGETC (CHOPT, NCH)
        CALL CDPURG (PATHN, KYDAT, KYTIM, CHOPT, IRC)
        NCH    = LENOCC (PATHN)
        IF (NCH.GT.40) NCH = 40
        IARGCD(1) = IQUEST(2)
        IARGCD(2) = IRC
        CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : CDPURG deletes '',I6,'' '//
     +       'objects in Path '//PATHN(1:NCH)//' return code '',I6)',
     +       IARGCD, 2)
*
      ELSE IF (CPATL.EQ.'CDPURK') THEN
*
*  **   CDPURK
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CHOPT, NCH)
        CALL CLTOU  (CHOPT)
        CALL VZERO  (IOKYCA, MXDMCK)
        IOPTB   = INDEX (CHOPT, 'B')
        IOPTF   = INDEX (CHOPT, 'F')
        IOPTK   = INDEX (CHOPT, 'K')
        IOPTS   = INDEX (CHOPT, 'S')
        IOPTX   = INDEX (CHOPT, 'X')
        CALL CDXOPT (CHOPT, IOKYCA)
        CHOPT   = ' '
        I0      = 0
        IF (IOPTB.EQ.1) THEN
          I0           = I0 + 1
          CHOPT(I0:I0) = 'B'
        ENDIF
        IF (IOPTF.EQ.1) THEN
          I0           = I0 + 1
          CHOPT(I0:I0) = 'F'
        ENDIF
        IF (IOPTK.EQ.1) THEN
          I0           = I0 + 1
          CHOPT(I0:I0) = 'K'
        ENDIF
        IF (IOPTS.EQ.1) THEN
          I0           = I0 + 1
          CHOPT(I0:I0) = 'S'
        ENDIF
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.EQ.0) THEN
          PATHN  = PAT1CT
          CALL CDKEYT
          IF (IOPTF.NE.0) THEN
            IPRBCA = 0
            IPRECA = 0
          ENDIF
          DO 25 I = 1, NPARCD
            IF (IOPTK.EQ.0.AND.(IPRBCA.EQ.0.AND.IPRECA.EQ.0)) THEN
              IF (IOPTX.NE.0) THEN
                WRITE (CHPRO, 1003) 'YYMMDD'
                CALL KUPROI (CHPRO, IDATE)
                WRITE (CHPRO, 1003) 'HHMMSS'
                CALL KUPROI (CHPRO, ITIME)
                CALL CDPKTS (IDATE, ITIME, ISEL(I), IRC)
              ELSE
                WRITE (CHPRO, '(''Selection Key '',I2)') I
                CALL KUPROI (CHPRO, ISEL(I))
              ENDIF
            ELSE
              ISEL(I) = 0
              IOKYCA(NOF1CK+2*I-1) = 0
              IOKYCA(NOF1CK+2*I)   = 0
            ENDIF
   25     CONTINUE
          IF (IOPTK.NE.0) THEN
            CALL VZERO (MASKCX, 100)
            MASKCX(IDHKSN) = 1
          ELSE
            CALL UCOPY (IOKYCA, MASKCX, NWKYCK)
          ENDIF
          DO 30 I = 1, MIN(NWKYCK,29)
            IF (MASKCX(I).NE.0) THEN
              IF (I.GT.NOF1CK.AND.I.LE.NOF1CK+2*NPARCD.AND.
     +            IOPTX.NE.0) THEN
                IF (I.LE.NOF1CK+NPARCD) THEN
                  CTEMP  = 'Begin'
                ELSE
                  CTEMP  = 'End'
                ENDIF
                WRITE (CHPRO, 1004) 'YYMMDD', CTEMP
                CALL KUPROI (CHPRO, IDATE)
                WRITE (CHPRO, 1004) 'HHMMSS', CTEMP
                CALL KUPROI (CHPRO, ITIME)
                CALL CDPKTS (IDATE, ITIME, KEYSCX(I), IRC)
              ELSE IF (I.EQ.IDHINS) THEN
                WRITE (CHPRO, 1005) 'YYMMDD', CTEMP
                CALL KUPROI (CHPRO, IDATE)
                WRITE (CHPRO, 1005) 'HHMM',   CTEMP
                CALL KUPROI (CHPRO, ITIME)
                CALL CDPKTM (IDATE, ITIME, KEYSCX(I), IRC)
              ELSE
                WRITE (CHPRO, 1006) I
                IF (IOTYCK(I).LE.2) THEN
                  CALL KUPROI (CHPRO, KEYSCX(I))
                ELSE
                  CALL KUPROC (CHPRO, CTEMP, NCH)
                  CALL UCTOH  (CTEMP, KEYSCX(I), 4, 4)
                ENDIF
              ENDIF
            ENDIF
   30     CONTINUE
          CALL CDPURK (PATHN, ISEL, MASKCX, KEYSCX, CHOPT, IRC)
          NCH    = LENOCC (PATHN)
          IF (NCH.GT.40) NCH = 40
          IARGCD(1) = IQUEST(2)
          IARGCD(2) = IRC
          CALL CDPRNT (L3PRDX, '(/,'' CDAUXI : CDPURK deletes '',I6'//
     +         ','' objects in Path '//PATHN(1:NCH)//' return code'//
     +         ' '',I6)', IARGCD, 2)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDPURP') THEN
*
*  **   CDPURP
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETI (IKEEP)
        CALL CDPURP (PATHN, IKEEP, ' ', IRC)
        IARGCD(1) = IKEEP
        IARGCD(2) = IRC
        CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : all but'',I6,'' partitio'//
     +       'ns deleted from '//PATHN(1:60)//' return code '',I6)',
     +       IARGCD, 2)
*
      ELSE IF (CPATL.EQ.'CDRALI') THEN
*
*  **   CDRALI
*
        CALL KUGETC (ALIAS, NCH)
        CALL CDRALI (ALIAS, PATHN, IRC)
        CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Path name for alias '//
     +       ALIAS//' is '//PATHN//''')', IARGCD, 0)
*
      ELSE IF (CPATL.EQ.'CDRDIC') THEN
*
*  **   DBRDIC
*
        CALL KUGETC (PATHN, NCH)
        CALL CDRDIC (PATHN, IRC)
        CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : dictionary recreated for'//
     +       ' '//PATHN(1:NCH)//' with return code '',I6)', IRC, 1)
*
      ELSE IF (CPATL.EQ.'CDRENK') THEN
*
*  **   CDRENK
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETI (KYI)
        CALL KUGETI (KYEL)
        CALL KUGETC (CHOPT, NCH)
        IOPTX   = INDEX (CHOPT, 'X')
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.EQ.0) THEN
          PATHN  = PAT1CT
          KST    = NWKYCK + 1
          CALL CDKEYT
          IF (NKEYCK.NE.0)  THEN
            IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
          ELSE
            IOPTP  = 0
          ENDIF
          IF (IOPTP.NE.0) THEN
            NCHAR  = LENOCC (PATHN)
            NKEYS  = NKEYCK
            DO 45 JK = 1, NKEYS
              IK     = NKEYS + 1 - JK
              KPNT   = IUHUNT (IK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                         NKEYS*KST, KST)
              IF (KPNT.GT.0) THEN
                IPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
              ELSE
                IPNT   = KOFSCD + LCDRCD + IKDRCD + (NKEYCK - 1) * KST
              ENDIF
              IF (IQ(IPNT+MOBJCD).GT.KYI) GO TO 45
              CALL CDPATH (TOP2CT, IK)
              CALL RZCDIR (TOP2CT, ' ')
              IF (IQUEST(1).NE.0) THEN
                PAT2CT = PATHN(1:NCHAR)//'/'//TOP2CT
                CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Illegal Path name '
     +               //PAT2CT//''')', IARGCD, 0)
                GO TO 999
              ENDIF
              NKEYCK = IQUEST(7)
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
              KK     = IUHUNT (KYI, IQ(KOFSCD+LCDRCD+IKDRCD+IDHKSN),
     +                         NKEYCK*KST, KST)
              IF (KK.GT.0) THEN
                KK     = (KK - IDHKSN) / KST + 1
                GO TO 50
              ELSE
                CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Object '',I12'//
     +               ','' not found in '//PATHN//''')', KYI, 1)
                GO TO 999
              ENDIF
   45       CONTINUE
            CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Object '',I12'//
     +           ','' not found in '//PATHN//''')', KYI, 1)
            GO TO 999
          ELSE
            KK     = IUHUNT (KYI, IQ(KOFSCD+LCDRCD+IKDRCD+IDHKSN),
     +                       NKEYCK*KST, KST)
            IF (KK.GT.0) THEN
              KK     = (KK - IDHKSN) / KST + 1
            ELSE
              CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Object '',I12'//
     +             ','' not found in '//PATHN//''')', KYI, 1)
              GO TO 999
            ENDIF
          ENDIF
   50     CALL CDKEYR (KK, NWKYCK, KEYSCX)
          CALL UCOPY  (KEYSCX, KEYXS, NWKYCK)
          IF (KYEL.GT.0.AND.KYEL.LE.NWKYCK.AND.KYEL.NE.IDHKSN.AND.
     +        KYEL.NE.IDHPTR.AND.KYEL.NE.IDHFLG.AND.KYEL.NE.IDHINS) THEN
            IF (KYEL.GT.NOF1CK.AND.KYEL.LE.NOF1CK+2*NPARCD.AND.
     +          IOPTX.NE.0) THEN
              IF (KYEL.LE.NOF1CK+NPARCD) THEN
                CTEMP  = 'Begin'
              ELSE
                CTEMP  = 'End'
              ENDIF
              WRITE (CHPRO, 1004) 'YYMMDD', CTEMP
              CALL KUPROI (CHPRO, IDATE)
              WRITE (CHPRO, 1004) 'HHMMSS', CTEMP
              CALL KUPROI (CHPRO, ITIME)
              CALL CDPKTS (IDATE, ITIME, KEYXS(KYEL), IRC)
            ELSE
              WRITE (CHPRO, 1006) KYEL
              IF (IOTYCK(KYEL).LE.2) THEN
                CALL KUPROI (CHPRO, KEYXS(KYEL))
              ELSE
                CALL KUPROC (CHPRO, CTEMP, NCH)
                CALL UCTOH  (CTEMP, KEYXS(KYEL), 4, 4)
              ENDIF
            ENDIF
            CALL CDRENK (PATHN, KEYSCX, KEYXS, IRC)
            IARGCD(1) = KYEL
            IARGCD(2) = KYI
            IARGCD(3) = IRC
            CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Key '',I4,'' changed'//
     +           ' for Object '',I12,'' in '//PATHN(1:40)//' return c'//
     +           'ode '',I6)', IARGCD, 3)
          ELSE
            IARGCD(1) = KYEL
            IARGCD(2) = NWKYCK
            CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Key '',I4,'' illegal'//
     +           ' for CDRENK - NWKYCK '',I8)', IARGCD, 2)
          ENDIF
        ELSE
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Illegal Path name '//
     +         PATHN//''')', IARGCD, 0)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDRHLP') THEN
*
*  **   CDRHLP
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CFNAM, NCH)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL CDOPFL (LUKYCX, CFNAM, 'UNKNOWN', ISTAT)
        IF (ISTAT.EQ.0) THEN
          CALL CDRHLP (PATHN, LUKYCX, IERR)
          IF (IERR.NE.0) CALL CDPRNT (LUKYCX, '(/,'' CDAUXI : Error'//
     +    ' '',I12,'' in getting help info. for '',/,'' '//PATHN(1:72)
     +    //''')', IERR, 1)
          CALL CDCLFL (LUKYCX)
          CALL KUEDIT (CFNAM, IST)
        ELSE
          CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : error '',I12,'' in ope'//
     +         'ning file '//CFNAM(1:NCH)//''')', ISTAT, 1)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDRNAM') THEN
*
*  **   CDRNAM
*
        CALL KUGETC (PATHN, NCH)
        NWMAX  = NOBJM
        CALL CDRNAM (PATHN, NWMAX, CTAG, IRC)
        CFMT(1:1)     = '('
        CFMT(120:120) = ')'
        DO 60 I1 = 1, NWMAX, 5
          I2     = I1 + 4
          IF (I2.GT.NWMAX) I2 = NWMAX
          CFMT(2:119) = ' '
          DO 55 I = I1, I2
            IARGCD(I-I1+1) = I
            K1             = (I - I1) * 23 + 2
            K2             = K1 + 22
            CFMT(K1:K2)    = ''' Tag '',I2,''         '','
            CFMT(K1+13:K1+20) = CTAG(I)
   55     CONTINUE
          NARG   = I2 - I1 + 1
          CALL CDPRNT (L3PRCX, CFMT, IARGCD, NARG)
   60   CONTINUE
*
      ELSE IF (CPATL.EQ.'CDEXTR'.OR.CPATL.EQ.'CDRTFZ') THEN
*
*  **   CDEXTR/CDRTFZ
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETI (LUNFZ)
        CALL KUGETC (CHOPT, NCH)
        CALL UOPTC  (CHOPT, 'R', IOPR)
        CALL UOPTC  (CHOPT, 'I', IOPI)
        IF (IOPI.NE.0.OR.IOPR.NE.0) THEN
          CHPRO  = 'Date of insertion (YYMMDD)'
          CHOP   = 'Time of insertion   (HHMM)'
        ELSE
          CHPRO  = 'Serial # of fisrt object'
          CHOP   = 'Serial # of last  object'
        ENDIF
        CALL KUPROI (CHPRO, K1MIN)
        CALL KUPROI (CHOP,  K1MAX)
        IF (CPATL.EQ.'CDRTFZ') THEN
          CALL CDRTFZ (PATHN, LUNFZ, K1MIN, K1MAX, CHOPT, IRC)
        ELSE
          CALL CDEXTR (PATHN, LUNFZ, K1MIN, K1MAX, CHOPT, IRC)
        ENDIF
        NCH    = LENOCC (PATHN)
        IF (NCH.GT.36) NCH = 36
        NCD    = LENOCC (CHOPT)
        IF (NCD.LT.1)  NCD = 1
        IARGCD(1) = IQUEST(2)
        IARGCD(2) = K1MIN
        IARGCD(3) = K1MAX
        IARGCD(4) = LUNFZ
        IARGCD(5) = IRC
        CALL CDPRNT (L3PRCX, '(/,'' CDAUXI : Transfers '',I8,'' objec'//
     +       'ts from '//PATHN(1:NCH)//' with CHOPT '//CHOPT(1:NCD)//
     +       ' in range '',I7,'' - '',I8,/,10X,''to unit '',I4'//
     +       ','' return code '',I6)', IARGCD, 5)
*
      ELSE IF (CPATL.EQ.'CDSHOW') THEN
*
*  **   CDSHOW
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CHOPT, NCH)
        CALL CDPRIN (PATHN, CHOPT, IRC)
*
      ELSE IF (CPATL.EQ.'CDSUMY') THEN
*
*  **   CDSUMY
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETI (NLEVL)
        CALL CDSUMY (PATHN, NLEVL, IRC)
*
      ELSE IF (CPATL.EQ.'CDUTIM') THEN
*
*  **   CDUTIM
*
        CALL KUGETI (IDATM)
        CALL KUGETC (CHOPT, NCH)
        IOPTM  = INDEX (CHOPT, 'M')
        IF (IOPTM.EQ.0) THEN
          CALL CDUPTS (IARGCD(2), IARGCD(3), IDATM, IRC)
        ELSE
          CALL CDUPTM (IARGCD(2), IARGCD(3), IDATM, IRC)
        ENDIF
        IARGCD(1) = IDATM
        CALL CDPRNT (L3PRCX, '(/,2X,I10,'' is the packed integer for '//
     +       'date and time : '',2I10,/)', IARGCD, 3)
*
      ELSE IF (CPATL.EQ.'CDVIEW') THEN
*
*  **   CDVIEW
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CFNAM, NCF)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL KUGETC (CHOPT, NCH)
        CALL UOPTC  (CHOPT, 'X', IOPXCA)
        CALL CDVIEW (CFNAM, PATHN, IRC)
*
      ELSE IF (CPATL.EQ.'CDWRITE') THEN
*
*  **   CDWRITE
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETC (CFNAM, NCF)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL KUGETC (CHOPT, NCH)
        CALL UOPTC  (CHOPT, 'X', IOPXCA)
        CALL KUPROC ('Create Directory ? (Y/N)', YESNO, NCH)
        IF (YESNO(1:1).EQ.'Y') THEN
          CALL KUPROI ('Number of user keys ', NKEX)
          CALL KUPROC ('Character option    ', CHOPT, NCP)
          CALL CDCRDR (PATHN, NKEX, CHOPT, IRC)
          IF (IRC.NE.0) GO TO 999
        ENDIF
        CALL KUPROC ('Keys Insert/Update ? (Y/N)', YESNO, NCH)
        IF (YESNO(1:1).EQ.'Y') THEN
          CALL KUPROC ('Horizontal or Vertical Mode ? (H/V)',
     +                  YESNO, NCH)
          CALL UOPTC (YESNO, 'H', IOPHCC)
          CALL CDEDKY (CFNAM, NCF, PATHN, 'A', IRC)
        ENDIF
*
      ENDIF
*
 1001 FORMAT ('Tag(',I2,')')
 1002 FORMAT (I2,'th Path name')
 1003 FORMAT (A,' of Validity time')
 1004 FORMAT (A,' of ',A,' Range')
 1005 FORMAT (A,' of Insertion time')
 1006 FORMAT ('Key(',I2,') ?')
*                                                             END CDAUXI
  999 END
