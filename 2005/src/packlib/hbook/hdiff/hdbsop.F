*
* $Id: hdbsop.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hdbsop.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 16/12/92  15.25.28  by  Rene Brun
*-- Author :    R. J. Genik II   23/10/92
      SUBROUTINE HDBSOP(TOL,NBINS,NBAD,DIFFS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Statistical comparison of 2 histograms
C-                           bin by bin (HDIFFB S option)
C-
C-   Inputs  : TOL,NBINS  max total number of bins
C-   Outputs : NBAD,DIFFS If DEBUG option on, various messages
C-   Controls: ACDIGT controls the number of significant digits in the
C-             small stat calc. Its default is 2, set in HDBINI
C-             (see below for more info)
C-
C-   Created   3-DEC-1990   James T. McKinley
C                           Michigan State University, USA
C-
C-   MODIFIED: 24-SEPT-92   R. J. Genik II
C-                          Michigan State University, USA
C-
C----------------------------------------------------------------------
C
C Local and passed variable declarations for HDIFFB S option
C
C----------------------------------------------------------------------
C
C
C
      INTEGER NBINS,NBAD,I,J,INDEX,N,NR
      REAL TOL,DIFFS(NBINS),SMLNUM,SUMRD
      REAL    MEANR,MEAND,ERRR,ERRD,CHI,HGCONT,FREQ
C
C          NOTE: R is contents of      ID1 = IDR = REFERENCE HISTOGRAM
C                D is contents of      ID2 = IDD = DATA HISTOGRAM
C                SIGR is error bars of ID1 = IDR = REFERENCE HISTOGRAM
C                SIGD is error bars of ID2 = IDD = DATA HISTOGRAM
C
      REAL    R,D,SIGR,SIGD
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DL,DU,P,QQQ,HBNSUM
#endif
#include "hbook/hcdifb.inc"
C
C
C======================================================================
C   Do comparisons
C======================================================================
C
C----------------------------------------------------------------------
C     Find the P and Q values used for binomial calculations.
C----------------------------------------------------------------------
C
      P=DBLE(1./(1.+LAMBDA))
      QQQ = 1.-P
C
      NBAD=0
      CHI=0.0
C
C
      DO 110 J = BEGINJ, ENDJ
        DO 100 I = BEGINI, ENDI
          R = HGCONT(IDR, I, J, 1)
C                                  ! Get value from Ref HG
          D = HGCONT(IDD, I, J, 1)
C                                  ! Value from Dat HG
C
C----------------------------------------------------------------------
C    Compute position in DIFFS
C----------------------------------------------------------------------
C
          INDEX = I - BEGINI + 1
          IF (TWODIM) INDEX = INDEX + XSIZ*(J - BEGINJ)
C
C----------------------------------------------------------------------
C    Default is to pass
C----------------------------------------------------------------------
          DIFFS(INDEX) = 1.0
 
C----------------------------------------------------------------------
C
C   Do actual comparisons.  NOTE: R = contents ID1 = IDR = REFERENCE HISTOGRAM
C                                 D = contents ID2 = IDD = DATA HISTOGRAM
C
C----------------------------------------------------------------------
C   Check for negative contents
C----------------------------------------------------------------------
C
          IF(R.LT.0.)THEN
            DIFFS(INDEX) = 0.0
C                             !absolute fail
            WRITE(DUMPDV,FMT=900) I,J,IDR
            GOTO 90
          ENDIF
          IF(D.LT.0.)THEN
            DIFFS(INDEX) = 0.0
C                             !absolute fail
            WRITE(DUMPDV,FMT=900) I,J,IDD
            GOTO 90
          ENDIF
C
C
C----------------------------------------------------------------------
C     If option Z has been selected and Ref or Dat bin = 0, skip bin
C----------------------------------------------------------------------
C
          IF((OPTS(ZEROS).EQ.1).AND.((R.EQ.0.).OR.(D.EQ.0))) THEN
            IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=400) I,J
C                                                         ! Debug opt
            GO TO 90
          ENDIF
C
C----------------------------------------------------------------------
C       If R=D=0 then skip the comp.
C----------------------------------------------------------------------
C
          IF((R+D).EQ.0.) THEN
            IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=410) 'S', I,J
            GO TO 90
          ENDIF
C
C======================================================================
C   Large Statistics (R>25 + D>25) + TOL > .001 or WEIGHTED
C======================================================================
C
          IF(((R.GE.25).AND.(D.GE.25).AND.(TOL.GE.0.001))
     +         .OR.WEIGHR.OR.WEIGHD)THEN
C
C----------------------------------------------------------------------
C         Compute mean values for R and D
C----------------------------------------------------------------------
C
            MEANR = (R + D)/(1. + LAMBDA)
            MEAND = (R + D)/(1. + 1./LAMBDA)
C
C----------------------------------------------------------------------
C        Compute SIGR and SIGD
C----------------------------------------------------------------------
C
            SIGR = HGCONT(IDR,I,J,2)
C                                    ! Get error bar for bin I for Ref
            SIGD = HGCONT(IDD,I,J,2)
C                                    ! Get error bar for bin I for Data
C
C----------------------------------------------------------------------
C         Compute the error values
C----------------------------------------------------------------------
C
            ERRR = SQRT((SIGR**2 + SIGD**2)/(1. + LAMBDA**2))
            ERRD = SQRT((SIGR**2 + SIGD**2)/(1. + 1./LAMBDA**2))
C
C
C
C----------------------------------------------------------------------
C         Trap the case ERRR or ERRD=0
C----------------------------------------------------------------------
C
            IF((ERRR.EQ.0.).OR.(ERRD.EQ.0.))THEN
              IF(R.EQ.MEANR.AND.D.EQ.MEAND)THEN
C                                                       ! Absolute pass
                IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=480) 'S',I,J
                GOTO 90
              ELSE
                DIFFS(INDEX)=0.0
C                                           ! Absolute fail
                IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=490) I,J
                GOTO 90
              ENDIF
C
C----------------------------------------------------------------------
C         Compute CHI = sqrt(chisq)
C----------------------------------------------------------------------
C
            ELSE
              CHI = SQRT(((R-MEANR)/ERRR)**2 + ((D-MEAND)/ERRD)**2)
            ENDIF
C
C----------------------------------------------------------------------
C         Find the probabilities.
C----------------------------------------------------------------------
C
            DIFFS(INDEX) = 2.-2.*FREQ(CHI)
C                                         ! From CERNLIB C300
C
C----------------------------------------------------------------------
C         Display debugging information if desired
C----------------------------------------------------------------------
C
            IF (OPTS(DEBUG). EQ. 1) THEN
              WRITE(DUMPDV,FMT=500) R, MEANR, ERRR
              WRITE(DUMPDV,FMT=510) D, MEAND, ERRD
              WRITE(DUMPDV,FMT=800) I,J,CHI**2,DIFFS(INDEX)
            ENDIF
          ELSE
C
C
C======================================================================
C   Small Statistics (R<=25 or D<=25) OR TOL < .001
C======================================================================
C
C           SUMRD is used in debug dump, but not in calc.
C
            SUMRD = R+D
C                                     ! Find the total # of trials
            N = INT(R + D + 0.5)
C                                     ! for rounding
            NR = INT(R + 0.5)
C
C----------------------------------------------------------------------
C         Find either the DL or the DU value (One with fewest terms)
C----------------------------------------------------------------------
C
C          Small Num is sent to HBNSUM as the requested accuracy
C          for the returned sum. (see HBNSUM for more details on
C          how this is accomplished)
C
C----------------------------------------------------------------------
C
            SMLNUM = TOL/(2.*10.**(ACDIGT+1.))
            DL=0.0
            DU=0.0
            IF (D.GT.R) THEN
              DL = HBNSUM(NR,0,N,P,QQQ,SMLNUM)
              DU = 1. + HBNSUM(NR,NR,N,P,QQQ,SMLNUM) - DL
            ELSE
              DU = HBNSUM(NR,N,N,P,QQQ,SMLNUM)
              DL = 1. + HBNSUM(NR,NR,N,P,QQQ,SMLNUM) - DU
            ENDIF
C
C----------------------------------------------------------------------
C         Calculate the DIFFS value
C----------------------------------------------------------------------
C
            DIFFS(INDEX) = 2*MIN(DL,DU)
C
C----------------------------------------------------------------------
C         Do the debugging dump
C----------------------------------------------------------------------
C
            IF (OPTS(DEBUG).EQ.1) THEN
              WRITE(DUMPDV,FMT=520) R,D,SUMRD
              WRITE(DUMPDV,FMT=530) DL,DU,P
            ENDIF
          ENDIF
   90     CONTINUE
          IF (DIFFS(INDEX).LT.TOL) NBAD=NBAD+1
          IF (OPTS(DEBUG).EQ.1) THEN
            WRITE(DUMPDV,FMT=810) I,J,DIFFS(INDEX),NBAD
          ENDIF
C
  100   CONTINUE
  110 CONTINUE
C
C======================================================================
C  Formats
C======================================================================
C----------------------------------------------------------------------
C     Special case indicators
C----------------------------------------------------------------------
C
  400 FORMAT('0','Reference bin ',I5,',',I3,
     +    '=0, Z opt, so bin passed')
  410 FORMAT('0','Ref=Dat=0 with opts ',A,' in bin ',I3,',',I3)
  480 FORMAT('0','Ref=Dat with opts ',A,' in bin ',I3,',',I3)
  490 FORMAT('0','S opt, Ref=Data error bar=0, thus bin ',I6,
     +  I6,   ' fails.')
C
C----------------------------------------------------------------------
C     S option data
C----------------------------------------------------------------------
C
  500 FORMAT('0','REF: Cont',E10.4,' Expect',E10.4,' EBar',E10.4)
  510 FORMAT(1X,'DAT: Cont',E10.4,' Expect',E10.4,' EBar',E10.4)
  520 FORMAT('0','REF',E10.4,' DAT',E10.4,' SumRD',E10.4)
  530 FORMAT(1X,'  L',E10.4,'   U',E10.4,'     P',E10.4)
  540 FORMAT(1X,'The ERR',A,' term = 0, thus only the ',A,
     +    ' term is computed.')
C
C----------------------------------------------------------------------
C     Result for each bin
C----------------------------------------------------------------------
C
  800 FORMAT(1X,'Bin ',I5,',',I4,': CHISQ',E10.4,' Diffs',E10.4)
  810 FORMAT(1X,'Bin ',I5,',',I4,': DIFFS',E10.4,' No. Bad',I4)
  900 FORMAT(1X,'Negative bin contents for BIN=',I6,',',I6,'ID=',I6,
     +    5X,'Negative bin contents not allowed for S and C options')
C
      END
