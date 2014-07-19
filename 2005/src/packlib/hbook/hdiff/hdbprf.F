*
* $Id: hdbprf.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hdbprf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 09/03/93  08.31.41  by  R. J. Genik II
*-- Author :    R. J. Genik II   23/10/92
      SUBROUTINE HDBPRF(TOL,NBINS,NBAD,DIFFS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compares 2 profile histograms bin by bin
C-                         (called by HDIFFB)
C-   Inputs  : TOL,NBINS
C-   Outputs : NBAD,DIFFS, If DEBUG option on, various messages
C-   Controls: None
C-
C-   Created   3-DEC-1990   James T. McKinley, Michigan State University, USA
C-   Modified 10-OCT-1992   R. J. Genik, MSU, USA
C-   Modified  8-MAR-1993   R. J. Genik, MSU, USA
C-     A-option now returns signed DIFFS values.
C----------------------------------------------------------------------
C  Local and passed variable declarations
C----------------------------------------------------------------------
C
      INTEGER NBINS,I,NBAD,NCR,NCD
      REAL TOL, DIFFS(NBINS)
      REAL ZVAL,T,TA,TB,HGCONT,FREQ,STUDIS
C
C          NOTE: R is contents of      ID1 = IDR = REFERENCE HISTOGRAM
C                D is contents of      ID2 = IDD = DATA HISTOGRAM
C                SIGR is error bars of ID1 = IDR = REFERENCE HISTOGRAM
C                SIGD is error bars of ID2 = IDD = DATA HISTOGRAM
C
      REAL    R,D,SIGR,SIGD
#include "hbook/hcprin.inc"
#include "hbook/hcdifb.inc"
C---------------------------------------------------------------------
C     Do the debug dump
C---------------------------------------------------------------------
C
      IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=250)
C
C---------------------------------------------------------------------
C     Begin looping
C---------------------------------------------------------------------
C
      NBAD = 0
      DO 100 I=BEGINI,ENDI
C
C---------------------------------------------------------------------
C     by default the bin passes
C---------------------------------------------------------------------
C
        IF (OPTS(AOPTN).EQ.1) THEN
          DIFFS(I)=0.0
C                                     ! A option pass
        ELSE
          DIFFS(I)=1.0
C                                     ! S or C opt pass
        ENDIF
C
C---------------------------------------------------------------------
C       Get some commonly used values from the histogram
C---------------------------------------------------------------------
C
        R = HGCONT(IDR,I,0,1)
        D = HGCONT(IDD,I,0,1)
        SIGR = HGCONT(IDR,I,0,2)
        SIGD = HGCONT(IDD,I,0,2)
        NCR = HGCONT(IDR,I,0,3) + .5
        NCD = HGCONT(IDD,I,0,3) + .5
C
C---------------------------------------------------------------------
C       Display the dubug info
C---------------------------------------------------------------------
C
        IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=450) R,D,SIGR,SIGD
C
C---------------------------------------------------------------------
C       Special case checks
C       If reference bin = 0 and Z opt, SKIP it
C---------------------------------------------------------------------
C
        IF ((OPTS(ZEROS).EQ.1).AND. R.EQ.0.0) THEN
          IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=400) I,0
          GO TO 90
        ENDIF
C
C=====================================================================
C     S option
C=====================================================================
C
        IF (OPTS(SOPTN).EQ.1) THEN
C
C---------------------------------------------------------------------
C       S option with Z option, skip if D = 0
C---------------------------------------------------------------------
C
          IF ((OPTS(ZEROS).EQ.1).AND. D.EQ.0.0) THEN
            IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=410) I,0
            GO TO 90
          ENDIF
C
C---------------------------------------------------------------------
C         We need to check if both error bars =0.
C---------------------------------------------------------------------
C
          IF ((SIGR+SIGD.EQ.0).AND.(R.NE.D)) THEN
            DIFFS(I)=0.0
C                                        ! Absolute fail
            IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=480) I
            GOTO 90
C
C---------------------------------------------------------------------
C         We need to check if R or D =0, must pass bin in this case
C---------------------------------------------------------------------
C
          ELSE IF ((R.EQ.0.).OR.(D.EQ.0.)) THEN
            IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=490) I
            GOTO 90
          ELSE
            IF(NCR+NCD.LE.2)THEN
C
C---------------------------------------------------------------------
C         We need to make sure number dof>0, if not must pass it
C---------------------------------------------------------------------
C
              IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=490) I
              GOTO 90
            ELSE
              TA = (REAL(NCR+NCD)/REAL(NCR*NCD))
            ENDIF
C
C---------------------------------------------------------------------
C           Check type of error bar, must have rms (spread) and not
C           std. dev. of mean for Student's t-Test
C---------------------------------------------------------------------
C
            IF (PSDMR) SIGR=SQRT(REAL(NCR))*SIGR
            IF (PSDMD) SIGD=SQRT(REAL(NCD))*SIGD
          ENDIF
C
C---------------------------------------------------------------------
C           Compute Test statistic for Student's t-Test
C---------------------------------------------------------------------
C
          TB = REAL((NCR-1)*SIGR**2+(NCD-1)*SIGD**2)/REAL(NCR+NCD-2)
          T = ABS(R-D)/SQRT(TA*TB)
C
C         Find the DIFFS values using Cern Lib G104
          DIFFS(I) = 2*(1.0 - STUDIS(T, NCR+NCD-2))
C
C=====================================================================
C     C option
C=====================================================================
C
        ELSEIF (OPTS(COPTN).EQ.1) THEN
          IF ((R.EQ.0.).OR.(D.EQ.0.)) THEN
            IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=500) I
            GOTO 90
          ENDIF
C
C---------------------------------------------------------------------
C       If error bar on R is zero in C opt, fail it if R.NE.D
C---------------------------------------------------------------------
C
          IF ( SIGR.EQ.0) THEN
            IF(R.NE.D)THEN
              DIFFS(I)=0.0
C                                          ! Absolute fail
              IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=470) I
            ELSE
              IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=420) 'C',I
            ENDIF
            GOTO 90
          ENDIF
C
C---------------------------------------------------------------------
C Check type of error bar
C---------------------------------------------------------------------
C
          IF (PSDMR) SIGR=SQRT(REAL(NCR))*SIGR
C                                              ! MUST HAVE RMS
          SIGD=SIGR/SQRT(REAL(NCD))
          IF (OPTS(DEBUG).EQ.1) THEN
            WRITE(DUMPDV,FMT=800) 'C OPTION: SD OF MEAN =',SIGD
          ENDIF
C
C---------------------------------------------------------------------
C         Compute the ZVAL  value (how many sigmas)
C---------------------------------------------------------------------
C
          ZVAL = ABS((D-R)/SIGD)
C
C---------------------------------------------------------------------
C         Find the DIFFS value using the FREQ routine
C---------------------------------------------------------------------
C
          DIFFS(I) = 2.-2.*FREQ(ZVAL)
C                                    ! CERNLIB C300
C
C---------------------------------------------------------------------
C         Do a debugging dump if optioned
C---------------------------------------------------------------------
C
          IF (OPTS(DEBUG).EQ.1) THEN
            WRITE(DUMPDV,FMT=650) I, ZVAL**2, DIFFS(I)
          ENDIF
C
C=====================================================================
C   A Option
C=====================================================================
C
        ELSEIF (OPTS(AOPTN).EQ.1) THEN
C
C---------------------------------------------------------------------
C       If error bar on R is zero in A opt, fail it if R.NE.D
C---------------------------------------------------------------------
C
          IF ( SIGR.EQ.0) THEN
            IF(R.NE.D)THEN
              DIFFS(I)=ABS(BIGP)
C                                          ! Absolute fail
              IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=460) I
              GO TO 90
            ELSE
              IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=420) 'A',I
            ENDIF
            GOTO 90
          ENDIF
          DIFFS(I) = (D-R)/SIGR
C                                          ! Value depends on user's
C                                          ! error bar spec. in HBPROF
        ENDIF
   90   CONTINUE
C
C=====================================================================
C  Check DIFFS
C=====================================================================
C
        IF (OPTS(AOPTN).EQ.1) THEN
          IF(ABS(DIFFS(I)).GT.TOL) NBAD=NBAD+1
        ELSE
          IF(DIFFS(I).LT.TOL) NBAD=NBAD+1
        ENDIF
C         Debug dump area
        IF (OPTS(DEBUG) .EQ. 1) THEN
          WRITE(DUMPDV,FMT=750) I, DIFFS(I), NBAD
        ENDIF
  100 CONTINUE
C
C=====================================================================
C  Formats
C=====================================================================
C
C---------------------------------------------------------------------
C     Initialization messages
C---------------------------------------------------------------------
C
  250 FORMAT(1X,'Both histograms are of profile type.')
C
C---------------------------------------------------------------------
C     Special case indicators
C---------------------------------------------------------------------
C
  400 FORMAT('0','Reference bin ',I5,',',I3,
     +    '=0, Z opt, so bin passed')
  410 FORMAT('0','Data bin ',I5,',',I3,
     +    '=0, Z opt and S option, so bin passed')
  420 FORMAT('0','Ref.=Data with opts ',A,' in bin ',I3)
  450 FORMAT('0',' h(y)',E10.4,' h(x)',E10.4,' e(y)',E10.4,' e(x)',
     +    E10.4)
  460 FORMAT('0','A opt and Ref. error bar= 0, thus bin ',I5,
     +    ' fails.')
  470 FORMAT('0','C opt and Ref. error bar= 0, thus bin ',I5,
     +    ' fails.')
  480 FORMAT('0','S opt, Ref.=Data error bar=0 thus bin ',I5,
     +    ' fails.')
  490 FORMAT('0','Ref or Dat=0 with S opt so bin ',I3, ' passes
     +        (no DATA is consistent with anything in this case)')
  500 FORMAT('0','Ref or Dat=0 with C opt so bin ',I3, ' passes
     +        (no DATA is consistent with anything in this case)')
C
  650 FORMAT(1X,'BIN',I5,' CHISQ',E10.4,' DIFFS',E10.4)
  750 FORMAT(1X,'BIN',I5,' DIFFS',E10.4,' NBAD',I5)
  800 FORMAT(A,E10.4,' (SEE HBPROF)')
C
      END
