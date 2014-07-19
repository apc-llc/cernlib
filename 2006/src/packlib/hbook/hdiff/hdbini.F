*
* $Id: hdbini.F,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: hdbini.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/03 23/07/93  17.46.20  by  Rene Brun
*-- Author :    R. J. Genik II   23/10/92
      SUBROUTINE HDBINI(ID1,ID2,TOL,NBINS,CHOPT,ERRORS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine for HDIFFB
C-                         1) set option flags and check for validity
C-                         2) set overall normalization
C-                         3) fill the local common block
C-
C-   Inputs  : IDR,IDD,TOL,NBINS,CHOPT
C-   Outputs : ERRORS, If DEBUG option on, various messages
C-              fills /HDBCOM/ the HDIFFB common block
C-   Controls: NONE
C-
C-   Created :  3-DEC-1990   James T. McKinley
C-                           Michigan State University, USA
C-
C-
C-   MODIFIED:  24-SEP-1992  R. J. Genik II
C-                           Michigan State University, USA
C-
C-
C---------------------------------------------------------------------
C
C
C
C----------------------------------------------------------------------
C     Local Variable Declaration
C----------------------------------------------------------------------
C
      INTEGER NBINS,ID1,ID2,NUMZSR,NUMZSD
      INTEGER LOCR,NCXR,NCYR,LEXCR,LEYCR,UEXCR,UEYCR,NWTR
      INTEGER LOCD,NCXD,NCYD,LEXCD,LEYCD,UEXCD,UEYCD,NWTD
      INTEGER LCIDR,LCIDD,LCONTR,LCONTD,LWR,LWD,JBIT
      INTEGER NENTR,NENTD,I,J,K,L,PROFIR,PROFID
      REAL    HGCONT,TOTALR,TOTALD,SUMR,SUMD,TOL
      LOGICAL ERRORS,NGCONR,NGCOND,ERBARR,ERBARD
      CHARACTER*80 TITLR,TITLD
      CHARACTER*(*) CHOPT
C
C
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcdifb.inc"
C
C=====================================================================C
C SECTION 1  -  Initialize variables, error check, set flags          C
C=====================================================================C
C   Initialize the variables
C
      ERRORS = .FALSE.
C                                     ! No errors yet
      WEIGHR = .FALSE.
C                                     ! Histogram 1 not weighted
      WEIGHD = .FALSE.
C                                     ! 2 is not either
      TWODIM = .FALSE.
C                                     ! Assume 1-D for now
      PROFIL = .FALSE.
C                                     ! Assume not profile for now
      PSDMR  = .TRUE.
C                                     ! Assume std deviation of mean for error
      PSDMD  = .TRUE.
C                                     ! bar, if it is a profile histogram
      NGCONR = .FALSE.
C                                     ! Negative contents flag
      NGCOND = .FALSE.
C                                     ! Negative contents flag
C
C----------------------------------------------------------------------
C  Change ID1 and ID2 to IDR and IDD to keep reference and data
C  straight internally.
C----------------------------------------------------------------------
C
      IDR=ID1
      IDD=ID2
C
C----------------------------------------------------------------------
C   Set the default output device for the debug dump (DUMPDV) to the
C   common default device (LOUT)
C----------------------------------------------------------------------
C
      DUMPDV = LOUT
C
C----------------------------------------------------------------------
C   Set the default accuracy for small S stat to 2 digits
C----------------------------------------------------------------------
C
      ACDIGT = 2.
C----------------------------------------------------------------------
C   Make sure the tolerance is not 0  (Warning)
C----------------------------------------------------------------------
C
      IF (TOL .LE. 0.) CALL HBUG('+Zero tolerance ',
     +  'HDIFFB',IDR)
C
C----------------------------------------------------------------------
C   Get booking information for IDR and IDD
C----------------------------------------------------------------------
C
      CALL HGIVE(IDR, TITLR, NCXR, LEXCR, UEXCR, NCYR, LEYCR, UEYCR,
     +    NWTR, LOCR)
      CALL HGIVE(IDD, TITLD, NCXD, LEXCD, UEXCD, NCYD, LEYCD, UEYCD,
     +    NWTD, LOCD)
C
C----------------------------------------------------------------------
C   Check that the histograms are of the same dimension.  If Y channels
C   are present, set flag for two dimensions.
C----------------------------------------------------------------------
C
      IF ((NCYR.EQ.0 .OR. NCYD.EQ.0) .AND. NCYR+NCYD.NE.0) THEN
        CALL HBUG('Both histograms must be the same dimension.',
     +      'HDIFFB', IDR)
        ERRORS=.TRUE.
      ENDIF
      IF (NCXR .NE. NCXD .OR. NCYR .NE. NCYD) THEN
        CALL HBUG('Number of channels is different.', 'HDIFFB', IDR)
        ERRORS=.TRUE.
      ENDIF
      IF (NCYR .GT. 0) TWODIM=.TRUE.
C
C----------------------------------------------------------------------
C   Decode option string
C----------------------------------------------------------------------
C
      CALL HUOPTC(CHOPT,OPTST,OPTS)
C
C----------------------------------------------------------------------
C  If A option is selected require that errors bars exist.
C----------------------------------------------------------------------
C
      CALL HFIND(IDR,'HDIFFB')
      LCIDR=LCID
      ERBARR = LQ(LQ(LCIDR-1)).NE.0
      IF((OPTS(AOPTN).EQ.1).AND..NOT.ERBARR) THEN
        CALL HBUG('A option with no error bars on reference histogram',
     +    'HDIFFB',IDR)
        ERRORS = .TRUE.
      ENDIF
C
C----------------------------------------------------------------------
C   Determine whether the histograms are of profile type
C   and if so, set profile flag.
C----------------------------------------------------------------------
C
C  Get address of IDR (REF) and decode flags I1-I230 (in HCBITS)
C
      CALL HDCOFL
      PROFIR=I8
      PROFIL = (PROFIR.EQ.1)
C   Find kind of error bar used for reference profile histograms
      IF (PROFIL) THEN
        LCONTR=LQ(LCIDR-1)
        LWR=LQ(LCONTR)
        PSDMR = JBIT(IQ(LWR),1).EQ.0
C                             !True if using spread instead of SD mean
C
      ENDIF
C
C  Get address of IDD (DAT) and decode flags I1-I230 (in HCBITS)
C
      CALL HFIND(IDD,'HDIFFB')
      LCIDD=LCID
      CALL HDCOFL
      PROFID=I8
C
C
      ERBARD = LQ(LQ(LCIDD-1)).NE.0
C   Find kind of error bar used for data profile histograms
      IF (PROFIL) THEN
        LCONTD=LQ(LCIDD-1)
        LWD=LQ(LCONTD)
        PSDMD = JBIT(IQ(LWD),1).EQ.0
      ENDIF
C
C----------------------------------------------------------------------
C   Check that both histograms are profile type
C----------------------------------------------------------------------
C
C
      IF (PROFIR.NE.PROFID) THEN
        CALL HBUG('Both histograms must be standard or profile type',
     +      'HDIFFB',IDR)
        ERRORS = .TRUE.
      ENDIF
C
C----------------------------------------------------------------------
C   Display options string and TOL to user for Debug Option
C----------------------------------------------------------------------
C
C
      IF (OPTS(DEBUG) .EQ. 1) THEN
        WRITE(DUMPDV, FMT=200) IDR, IDD
        WRITE(DUMPDV, FMT=210) CHOPT, TOL
      ENDIF
C
C----------------------------------------------------------------------
C   Options N,O,U,L,R,T,B not allowed for profile histograms
C   Check parameterization for OPTS index values (numbers to save space)
C----------------------------------------------------------------------
C
      IF((PROFIR+PROFID.NE.0).AND.((OPTS(1)+OPTS(3)+OPTS(4)+OPTS(9)+
     +    OPTS(10)+OPTS(11)+OPTS(12)).NE.0))THEN
        CALL HBUG('options N,O,U,L,R,T, or B used with profile hist.',
     +             'HDIFFB',IDR)
        ERRORS=.TRUE.
      ENDIF
C
C----------------------------------------------------------------------
C   Screen out incompatible options.  S,C, or A. (Warning)
C   Set S option as default.
C----------------------------------------------------------------------
C
      IF (OPTS(SOPTN) + OPTS(COPTN) + OPTS(AOPTN) .GT. 1) THEN
        CALL HBUG('Only one comparison at a time, please','HDIFFB',
     +      IDR)
        OPTS(SOPTN) = 1
C                                     ! Default to S case
        OPTS(COPTN) = 0
C                                     ! Turn off both the
        OPTS(AOPTN) = 0
C                                     ! C and A modes.
      ENDIF
C
C
C----------------------------------------------------------------------
C   If no test options are selected, default to S
C----------------------------------------------------------------------
C
      IF (OPTS(SOPTN)+OPTS(COPTN)+OPTS(AOPTN).EQ.0) OPTS(SOPTN)=1
C
C
C----------------------------------------------------------------------
C   Make sure both histograms have the same binning.  The routines that
C   return the content and error bars don't worry about the different
C   binning, but it may cause bad results, so warn user.
C----------------------------------------------------------------------
C
      IF (LEXCR .NE. LEXCD) THEN
        CALL HBUG('+Different binning ','HDIFFB',IDR)
      ENDIF
      IF (NCYR.NE.0.AND.LEYCR .NE. LEYCD) THEN
        CALL HBUG('+Different binning ','HDIFFB',IDR)
      ENDIF
C
C
C----------------------------------------------------------------------
C   Prepare for calculation of indices in DIFFS
C----------------------------------------------------------------------
C
      BEGINI = 1
C                                     ! First bin, no underflow, X
      ENDI = NCXR
C                                     ! Last bin, no overflow, X
C
      IF (TWODIM) THEN
        BEGINJ = 1
C                                     ! First bin, no underflow, Y
        ENDJ = NCYR
C                                     ! Last bin, no overflow, Y
      ELSE
        BEGINJ = 0
C                                     ! Zero bins
        ENDJ = 0
C                                     ! Zero is also the last bin
      ENDIF
C
C
C----------------------------------------------------------------------
C   Find the starting and ending points with O/U flow bins
C----------------------------------------------------------------------
C
      IF (OPTS(UFLOW).EQ.1.OR.OPTS(XUNDR).EQ.1) BEGINI = 0
      IF (OPTS(OFLOW).EQ.1.OR.OPTS(XOVER).EQ.1) ENDI = NCXR+1
      IF ((OPTS(UFLOW).EQ.1.OR.OPTS(YUNDR).EQ.1).AND.TWODIM) BEGINJ=0
      IF((OPTS(OFLOW).EQ.1.OR.OPTS(YOVER).EQ.1).AND.TWODIM)ENDJ=NCYR+1
C
C
C----------------------------------------------------------------------
C   Find the XSIZ value (total length across the X-Axis)
C----------------------------------------------------------------------
C
      XSIZ = ENDI - BEGINI + 1
C                                   ! Used in INDEX calculation
C
C
C----------------------------------------------------------------------
C   Verify that DIFFS has enough room for the output.
C----------------------------------------------------------------------
C
      K = XSIZ*(ENDJ-BEGINJ+1)
      IF (K.GT.NBINS)THEN
        CALL HBUG('Not enough bins in DIFFS to hold result','HDIFFB',
     +      IDR)
        WRITE(DUMPDV, FMT=910) K
        ERRORS = .TRUE.
      ENDIF
C
C
C----------------------------------------------------------------------
C   Jump ship if any errors have been detected
C----------------------------------------------------------------------
C
      IF (ERRORS) GO TO 999
C
C======================================================================
C   Section 2: Continue with the second stage of initialization.
C======================================================================
C
C
C----------------------------------------------------------------------
C   Find the sum of the weights for each histogram
C----------------------------------------------------------------------
C
C   Clear sums of number of bins with zero contents
C
      NUMZSR = 0
      NUMZSD = 0
C
C
      SUMR = 0.
C                                     ! Clear sums in "good" regions
      SUMD = 0.
      TOTALR =0.
C                                     ! Clear total sums
      TOTALD =0.
      L=0
C                                     ! Initialize L for 1-D
      IF (TWODIM) L=1
C                                     ! Add for Y overflow only on 2-D
C
      DO 41 J=0, NCYR+L
C                                     ! including O/U flows
        DO 40 I=0, NCXR+1
C                                     ! Sum each bin contents
          R = HGCONT(IDR,I,J,1)
C                                     ! Save content
          IF(R.LT.0.)NGCONR=.TRUE.
          TOTALR=TOTALR+R
C                                     ! Add to total sum
          IF(I.GE.BEGINI .AND. I.LE.ENDI .AND. J.GE.BEGINJ .AND. J.LE.
     +        ENDJ) THEN
            SUMR=SUMR+R
C                                     ! Add to sum if in requested region
C
C
            IF (R.EQ.0.)  NUMZSR = NUMZSR + 1
C
C                                     ! Add to number of zeroes
C
          ENDIF
   40   CONTINUE
   41 CONTINUE
C
      DO 46 J=0, NCYD+L
C                                     ! including O/U flows
        DO 45 I=0, NCXD+1
C                                     ! Sum each bin contents
          D = HGCONT(IDD,I,J,1)
C                                     ! Save content
          IF(D.LT.0.)NGCOND=.TRUE.
          TOTALD=TOTALD+D
C                                     ! Add to total sum
          IF(I.GE.BEGINI .AND. I.LE.ENDI .AND. J.GE.BEGINJ .AND. J.LE.
     +        ENDJ) THEN
            SUMD=SUMD+D
C                                     ! Add to sum if in requested region
C
C
            IF (D.EQ.0.)  NUMZSD = NUMZSD + 1
C
C                                     ! Add to number of zeroes
          ENDIF
   45   CONTINUE
   46 CONTINUE
C
C
C----------------------------------------------------------------------
C   If sum of contents is 0, exit
C----------------------------------------------------------------------
C
      IF ((SUMR .EQ. 0).AND.(.NOT.NGCONR)) THEN
        CALL HBUG('Sum of hitsogram contents is zero!', 'HDIFFB', IDR)
        ERRORS = .TRUE.
      ENDIF
      IF ((SUMD .EQ. 0).AND.(.NOT.NGCOND)) THEN
        CALL HBUG('Sum of hitsogram contents is zero!', 'HDIFFB', IDD)
        ERRORS = .TRUE.
      ENDIF
C
C
C----------------------------------------------------------------------
C   Find out if reference histogram is weighted.
C   Note: We flag as weighted if the number of entries is not equal
C         any of the below:
C
C         - Sum of contents (filled via HFILL)
C         - NBINS (filled via HPAK)
C         - NBINS - #zeroes (filled via HPAK, but had zeores in filling
C           array. In this case HNOENT returns number of non-zero bins)
C
C
C----------------------------------------------------------------------
C
      CALL HNOENT(IDR, NENTR)
      WEIGHR = ((REAL(NENTR) .NE. TOTALR).AND.(NENTR .NE. NBINS)
     +  .AND.(NENTR.NE.(NBINS-NUMZSR)) .AND. .NOT.PROFIL)
      IF (WEIGHR) THEN
C
        IF ((OPTS(3)+OPTS(4)+OPTS(9)+OPTS(10)+
     +      OPTS(11)+OPTS(12)).NE.0) THEN
          CALL HBUG('U/O/R/L/T/B Option with weighted events',
     +        'HDIFFB', IDR)
          ERRORS = .TRUE.
        ENDIF
C
        IF (TWODIM) THEN
          CALL HBUG(
     + 'Weighted or saturated 2-D histogram, results are unreliable!'
     + ,'HDIFFB',IDR)
        ELSEIF (.NOT.ERBARR) THEN
          CALL HBUG('Weighted events and no HBARX','HDIFFB',IDR)
        ENDIF
      ENDIF
C
C
C----------------------------------------------------------------------
C   Check if data histogram is weighted.
C----------------------------------------------------------------------
C
      CALL HNOENT(IDD, NENTD)
      WEIGHD = ((REAL(NENTD).NE.TOTALD).AND.(NENTD.NE.NBINS)
     +  .AND.(NENTD.NE.(NBINS-NUMZSD)).AND.(.NOT.PROFIL))
      IF (WEIGHD) THEN
C
        IF ((OPTS(3)+OPTS(4)+OPTS(9)+OPTS(10)+
     +      OPTS(11)+OPTS(12)).NE.0) THEN
          CALL HBUG('U/O/R/L/T/B Option with weighted events',
     +        'HDIFFB', IDD)
          ERRORS = .TRUE.
        ENDIF
C
        IF (TWODIM) THEN
          CALL HBUG(
     +'Weighted or saturated 2-D histogram, results are unreliable!'
     +,'HDIFFB',IDD)
        ELSEIF (.NOT.ERBARD) THEN
          CALL HBUG('Weighted events and no HBARX','HDIFFB',IDD)
        ENDIF
      ENDIF
C
C
C----------------------------------------------------------------------
C   Beam up if any errors have occured
C----------------------------------------------------------------------
C
      IF (ERRORS) GO TO 999
C
C
C
C----------------------------------------------------------------------
C  Find scaling factor for overall normalization difference between R+D
C----------------------------------------------------------------------
C
C
      IF (OPTS(NORMD).EQ.1) THEN
        LAMBDA = 1
C                                 ! Turn off scaling for N option
      ELSE
        LAMBDA = 1
        IF (SUMR.NE.0) LAMBDA = SUMD/SUMR
C                                 ! Scale to contents in desired region
      ENDIF
C
      IF (OPTS(DEBUG) .EQ. 1) THEN
C                                 ! Tell user weighted status for Debug
        WRITE(DUMPDV, FMT=220) WEIGHR,WEIGHD
        WRITE(DUMPDV, FMT=240) LAMBDA, SUMR, SUMD
C                                 ! Debugging dump
      ENDIF
 
C---------------------------------------------------------------------
C     Put LOG(bigp) - 1. into /HDBCOM/ used in routines to
C     avoid overflow. We use -1. because usually we will get a log
C     approxitmation to test for possible overflow before we
C     actually calculate the number.
C---------------------------------------------------------------------
C
      LNBIGP = LOG(ABS(REAL(BIGP))) - 1.
C
C======================================================================
C  Formats
C======================================================================
C
C
C
C----------------------------------------------------------------------
C     Initialization messages
C----------------------------------------------------------------------
C
  200 FORMAT('1','HDIFFB debugging dump is now on for histograms R=',
     +    I10,' + D=',I10,'.')
  210 FORMAT(1X,'Option string: ',A,'  TOL=',E10.4)
  220 FORMAT(1X,'Ref, Data histograms are of weighted type? ',2L2)
  240 FORMAT(1X,'Ratio',E10.4,' REF Cont',E10.4,' DAT Cont',E10.4)
C
C
C----------------------------------------------------------------------
C     Error message for not enough room in input arrays
C----------------------------------------------------------------------
C
  910 FORMAT(1X,'This histograms requires',I6,' bins for the result.')
C
C
  999 RETURN
      END
