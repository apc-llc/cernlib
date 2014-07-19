*
* $Id: hdbcop.F,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: hdbcop.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/01 28/10/93  08.26.07  by  R. J. Genik II
*-- Author :    R. J. Genik II   23/10/92
      SUBROUTINE HDBCOP(TOL,NBINS,NBAD,DIFFS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods: Statistical compatibility of data histogram
C-                        bin by bin with a reference histogram
C-                        (HDIFFB C option)
C-
C-   Inputs  : TOL,NBINS
C-   Outputs : NBAD,DIFFS, If DEBUG option on, various messages
C-   Controls: None
C-
C-   Created   3-DEC-1990   James T. McKinley, Michigan State University, USA
C-
C-
C-   MODIFIED  1-OCT-1992   R. J. Genik II, Michigan State University, USA
C-   MODIFIED 27-OCT-1993   R. J. Genik II, Michigan State University, USA
C-                          Divide by zero protection increased. See
C-                          comment below.
C-
C---------------------------------------------------------------------
C  Local variable declarations for HDIFFB C option
C---------------------------------------------------------------------
C
      INTEGER I,J,INDEX,ND
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DTEMPU,DTEMPL,DGAGNC,DGAPNC
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL GAGNC,GAPNC
#endif
      REAL    ZVAL,MEAND,L,U,HGCONT,PROB,FREQ,GAMDIS
C
C---------------------------------------------------------------------
C... passed varaibles, input and output
C---------------------------------------------------------------------
C
      INTEGER NBINS,NBAD
      REAL TOL,DIFFS(NBINS)
#include "hbook/hcdifb.inc"
C
C
C
      NBAD=0
      ZVAL=0.
      DO 110 J = BEGINJ, ENDJ
        DO 100 I=BEGINI, ENDI
          R = HGCONT(IDR, I, J, 1)
C                                  ! Get value from Ref HG
          D = HGCONT(IDD, I, J, 1)
C                                  ! Value from Dat HG
          INDEX = I - BEGINI + 1
C                                  ! Compute position in DIFFS
          IF (TWODIM) INDEX = INDEX + XSIZ*(J - BEGINJ)
C
C---------------------------------------------------------------------
C  default is to pass
C---------------------------------------------------------------------
C
          DIFFS(INDEX) = 1.0
C
C---------------------------------------------------------------------
C
C   Do actual comparisons.  NOTE: R = ID1 = IDR = REFERENCE HISTOGRAM
C                                 D = ID2 = IDD = DATA HISTOGRAM
C
C---------------------------------------------------------------------
C   Check for negative contents (and fail)
C---------------------------------------------------------------------
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
C---------------------------------------------------------------------
C       If option Z has been selected and Ref bin = 0, skip bin
C---------------------------------------------------------------------
C
          IF((OPTS(ZEROS).EQ.1) .AND. (R .EQ. 0)) THEN
            IF (OPTS(DEBUG) .EQ. 1) WRITE(DUMPDV,FMT=400) I,J
C                                                           ! Debug opt
            GO TO 90
          ENDIF
C
C---------------------------------------------------------------------
C       If R=D=0 then skip the comp.
C---------------------------------------------------------------------
C
          IF((R+D).EQ.0.) THEN
            IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=410) 'C', I,J
            GOTO 90
          ENDIF
C
C---------------------------------------------------------------------
C Get expected value for D, <D>=lambda*<R> (<R>=R)
C                           or =lambda*sum(Wr)
C---------------------------------------------------------------------
C
          MEAND = LAMBDA*R
C
C---------------------------------------------------------------------
C       This is the update referred to above
C       27 October 1993 - R. J. Genik II
C       The calculation of ZVAL will be done here, now, after we make
C       sure no divide by zero error will occur.
C       The check for negative contents assures that R and D are equal
C       to or greater than zero. Then, we are assured that the case
C       R=D=0 has been handled. If D=0.NE.R, the calculations and
C       functions called properly return the probability that 0
C       was seen when we expected a non-zero result. (The case
C       lambda = 0 is handled in HDBINI) The case R=0.NE.D is handled
C       below: we issue an absolute fail because the specification of
C       the routine states that the error bar is taken from the
C       reference histogram. We do not allow the user to specify an
C       error bar in this option, that is what the A-option is for.
C       e.g. the question: I expected zero and got 3 is not answered.
C---------------------------------------------------------------------
C
C---------------------------------------------------------------------
C         Calculate the SIGD value for Poisson
C---------------------------------------------------------------------
C
C
C
            SIGD=SQRT(MEAND)
C
            IF((SIGD.EQ.0.).AND.(D.NE.R))THEN
              DIFFS(INDEX)=0.0
C                                               ! Absolute fail
              IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=470) I
              GOTO 90
            ENDIF
C
C----------------------------------------------------------------------
C         Compute ZVAL (how many sigmas), used to determine
C         level of accuracy required and in Large Stat case
C----------------------------------------------------------------------
C
            ZVAL=(D-MEAND)/SIGD
C
C---------------------------------------------------------------------
C
C======================================================================
C   Large statistics or weighted
C======================================================================
C
          IF((MEAND.GT.1000000).AND.(TOL.GE.0.01)
     +        .OR.(WEIGHD))THEN
C
            ZVAL=ABS(ZVAL)
C                         ! for Large stat, we remove the sign
C
C----------------------------------------------------------------------
C    Here is where the approximation is:
C
C         We assume that Poisson ==> Gaussian
C           Hence, we have symmetric distribution with
C           standard deviation of sqrt(mean), or sqrt(sum w**2),
C           and then use z value as the test statistic.
C
C
C---------------------------------------------------------------------
C         Compute the probabilities
C---------------------------------------------------------------------
C
            DIFFS(INDEX) = 2. - 2.*FREQ(ZVAL)
C
C----------------------------------------------------------------------
C         Do a debugging dump if requested
C----------------------------------------------------------------------
C
            IF (OPTS(DEBUG).EQ.1) THEN
              WRITE(DUMPDV,FMT=600) R, D, MEAND, SIGD
              WRITE(DUMPDV,FMT=800)I,J,ZVAL**2,DIFFS(INDEX)
            ENDIF
          ELSE
C
C======================================================================
C   Small statistics
C======================================================================
C
            ND = INT(D + 0.5)
C                       !avoid roundoff problems
C
C----------------------------------------------------------------------
C If we have small tol, are far away from the mean, and are within
C  the range of called function accuracy, do more accurate L, U
C  calculation.
C----------------------------------------------------------------------
C
            IF (ND.EQ.0) THEN
              L = EXP(-MEAND)
            ELSE
              IF ((TOL.LT.0.0001).AND.(ZVAL.LT.-3.)) THEN
#if defined(CERNLIB_DOUBLE)
                DTEMPL = DGAGNC(REAL(ND)+1.,MEAND)
#endif
#if !defined(CERNLIB_DOUBLE)
                DTEMPL = GAGNC(REAL(ND)+1.,MEAND)
#endif
                L = REAL(DTEMPL)
              ELSE
                L = PROB(2.*MEAND, (2*ND+ 2))
              ENDIF
            ENDIF
C                                          ! Get lower tail probability
            IF ((TOL.LT.0.0001).AND.(ZVAL.GT.3.).AND.
     +         (MEAND).LT.10000.) THEN
C
C----------------------------------------------------------------------
C          - The above avoids convergence error in DGAPNC
C----------------------------------------------------------------------
C
#if defined(CERNLIB_DOUBLE)
              DTEMPU = DGAPNC(REAL(ND),MEAND)
#endif
#if !defined(CERNLIB_DOUBLE)
              DTEMPU = GAPNC(REAL(ND),MEAND)
#endif
              U = REAL(DTEMPU)
            ELSE
              IF (ND.EQ.0) THEN
                U = 1.
C
C----------------------------------------------------------------------
C          - The below avoids convergence error in GAMDIS
C----------------------------------------------------------------------
C
              ELSE IF ((ZVAL.GT.2.0).AND.(MEAND.LT.5000.)) THEN
                U = GAMDIS(MEAND,REAL(ND))
              ELSE
C
C----------------------------------------------------------------------
C           - PROB uses 26.4.14 from Abramowitz, et al, for large
C             values of mean and degrees of freedom. Accurate to
C             3 digits for dof.gt.100, and it doesn't generate errors
C             for our guarenteed range.
C----------------------------------------------------------------------
C
                U = 1. - PROB(2.*MEAND,2*ND)
              ENDIF
            ENDIF
C
C----------------------------------------------------------------------
C  Calculate DIFFS. Use smallest tail * 2
C----------------------------------------------------------------------
C
            DIFFS(INDEX)=2.*MIN(L,U)
C
C
C---------------------------------------------------------------------
C       Display debugging dump if requested
C---------------------------------------------------------------------
C
            IF (OPTS(DEBUG).EQ.1) THEN
              WRITE(DUMPDV, FMT=610) R,D,L,U
            ENDIF
          ENDIF
   90     CONTINUE
C
C----------------------------------------------------------------------
C   Check if bin is below user's tol level
C----------------------------------------------------------------------
C
          IF(DIFFS(INDEX) .LT. TOL) NBAD=NBAD+1
C
C---------------------------------------------------------------------
C       Display debugging dump if requested
C---------------------------------------------------------------------
C
          IF (OPTS(DEBUG).EQ.1) THEN
            WRITE(DUMPDV, FMT=810) I,J,DIFFS(INDEX),NBAD
          ENDIF
  100   CONTINUE
  110 CONTINUE
C
C
C---------------------------------------------------------------------
C  Formats
C---------------------------------------------------------------------
C
C     Special case indicators and debug dump info...
C---------------------------------------------------------------------
  400 FORMAT('0','Reference bin ',I6,',',I6,
     +    '=0, Z opt, so bin passed')
  410 FORMAT('0','Ref=Dat=0 with opts ',A,' in bin ',I6,',',I6)
  470 FORMAT('0','C opt and Ref. error bar= 0, thus bin ',I6,
     + I6,' fails.')
C
C     C option data
  600 FORMAT('0','REF',E10.4,' DAT',E10.4,' Expect',E10.4,' EBar',E10.
     +    4)
  610 FORMAT('0','REF',E10.4,' DAT',E10.4,' L',E10.4,' U',E10.4)
C
C     Result for each bin
  800 FORMAT(1X,'Bin ',I6,',',I6,': CHISQ',E10.4,' Diffs',E10.4)
  810 FORMAT(1X,'Bin ',I6,',',I6,': DIFFS',E10.4,' No. Bad',I6)
  900 FORMAT(1X,'Negative bin contents for BIN=',I6,',',I6,'ID=',I6,
     +    5X, 'Negative bin contents not allowed for S and C options')
C
      END
