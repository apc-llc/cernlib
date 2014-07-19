*
* $Id: hdbaop.F,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: hdbaop.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 09/03/93  08.29.18  by  R. J. Genik II
*-- Author :    R. J. Genik II   23/10/92
      SUBROUTINE HDBAOP(TOL,NBINS,NBAD,DIFFS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Absolute comparison of 2 histograms bin
C-                          by bin (HDIFFB A option)
C-                          Must have 1d histogram with error bars
C-
C-   Inputs  : TOL,NBINS
C-   Outputs : NBAD,DIFFS, If DEBUG option on, various messages
C-   Controls: None
C-
C-
C-   Created :  3-DEC-1990   James T. McKinley
C-                           Michigan State University, USA
C-
C-
C-   MODIFIED:  24-SEP-1992  R. J. Genik II
C-                           Michigan State University, USA
C-
C-
C-   MODIFIED:   8-MAR-1993  R. J. Genik II
C-                           Michigan State University, USA
C-        A_OPTION now returns signed DIFFS values
C-
C----------------------------------------------------------------------
C Local and passed variable declarations
C----------------------------------------------------------------------
C
      INTEGER NBINS,NBAD,I,INDEX
      REAL    TOL,DIFFS(NBINS),HGCONT
C
C          NOTE: R is contents of      ID1 = IDR = REFERENCE HISTOGRAM
C                D is contents of      ID2 = IDD = DATA HISTOGRAM
C                SIGR is error bars of ID1 = IDR = REFERENCE HISTOGRAM
C
      REAL    R,D,SIGR
C
C
#include "hbook/hcprin.inc"
#include "hbook/hcdifb.inc"
C---------------------------------------------------------------------
C    Begin calculation
C---------------------------------------------------------------------
C
      NBAD=0
      DO 100 I=BEGINI, ENDI
        R = HGCONT(IDR, I, 0, 1)
C                                  ! Get value from Ref HG
        D = HGCONT(IDD, I, 0, 1)
C                                  ! Value from Dat HG
C---------------------------------------------------------------------
C   Compute position in DIFFS
C---------------------------------------------------------------------
C
        INDEX = I - BEGINI + 1
C
C---------------------------------------------------------------------
C  default is to pass; in this case, 0 s.d. away means pass
C---------------------------------------------------------------------
C
        DIFFS(INDEX) = 0
C
C---------------------------------------------------------------------
C   Do actual comparisons.  NOTE: R = ID1 = IDR = REFERENCE HISTOGRAM
C                                 D = ID2 = IDD = DATA HISTOGRAM
C---------------------------------------------------------------------
C
C---------------------------------------------------------------------
C   If option Z has been selected and Ref bin = 0, skip bin
C---------------------------------------------------------------------
C
        IF((OPTS(ZEROS).EQ.1) .AND. (R .EQ. 0)) THEN
          IF (OPTS(DEBUG) .EQ. 1) WRITE(DUMPDV,FMT=400) I,0
C                                                          ! Debug opt
          GO TO 100
        ENDIF
C
        SIGR = HGCONT(IDR,I,0,2)
C
C---------------------------------------------------------------------
C       If SIGR = 0 we will get divide by zero error.
C       Check SIGR, and decide whether or not to pass the bin.
C---------------------------------------------------------------------
C
        IF ((SIGR.EQ.0).OR.(R.EQ.0)) THEN
          IF (D.EQ.R*LAMBDA) THEN
            IF(OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=410) 'A',I,0
          ELSE
            DIFFS(INDEX)=ABS(BIGP)
C                                         ! Set DIFFS to a very large
            IF (OPTS(DEBUG).EQ.1) WRITE(DUMPDV,FMT=420) I,0
          ENDIF
C
        ELSE
C
C---------------------------------------------------------------------
C             Note: this can overflow the machine, but for the
C             promised range of 10**9, sigd would have to be
C             10**-29, which is unreasonable.
C---------------------------------------------------------------------
C
          DIFFS(INDEX)=(D-LAMBDA*R)/(LAMBDA*SIGR)
C                                                     ! Compute diffs
        ENDIF
        IF (ABS(DIFFS(INDEX)) .GT. TOL) NBAD=NBAD+1
C
C---------------------------------------------------------------------
C         Display debugging information
C---------------------------------------------------------------------
C
        IF(OPTS(DEBUG).EQ.1) THEN
          WRITE(DUMPDV,FMT=700) R, D, LAMBDA*R, LAMBDA*SIGR
          WRITE(DUMPDV,FMT=810) I,0,DIFFS(INDEX),NBAD
        ENDIF
  100 CONTINUE
C
C=====================================================================
C  Formats
C=====================================================================
C
C---------------------------------------------------------------------
C     Special case indicators
C---------------------------------------------------------------------
C
  400 FORMAT('0','Reference bin ',I5,',',I3,
     +    '=0, Z opt, so bin passed')
  410 FORMAT('0','Ref=Dat=0 with opts ',A,' in bin ',I3,',',I3)
  420 FORMAT('0','Reference bin',I5,',',I3,
     +
     +': Cont=0, A opt, no Z opt, DIFFS is undefined so bin fails.')
  460 FORMAT('0','A opt and Ref. error bar= 0, thus bin ',I5,
     +    ' fails.')
C
C     A option data
  700 FORMAT('0','REF',E10.4,' DAT',E10.4,' Expect',E10.4,' EBar',E10.
     +    4)
C
  810 FORMAT(1X,'Bin ',I5,',',I4,': DIFFS',E10.4,' No. Bad',I4)
C
      END
