*
* $Id: setup.F,v 1.1.1.1 1996/01/11 14:15:05 mclareni Exp $
*
* $Log: setup.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:05  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE SETUP
C     ****************
C
C          USER ROUTINE TO BOOK HISTOGRAMS.
C          HISTOGRAM I IS UNWEIGHTED AND NEVER PRINTED.
C          HISTOGRAM I+NHSHFT IS WEIGHTED BY SIGF/NEVENT UNLESS SIGF=0.
C          SCATTER PLOTS ARE AUTOMATICALLY SCALED.
C
#include "cojets/itapes.inc"
      COMMON/MXHIST/MXHIST,NHSHFT
      LOGICAL HEXIST
C
C          MXHIST=MAXIMUM HISTOGRAM ID.
      MXHIST=100
      NHSHFT=100
C
C
C          BOOK HISTOGRAMS WITH HBOOK1 AND HBOOK2 BELOW.
C          ID SHOULD BE LESS THAN MXHIST.
C-----------------------------------------------------------------------
C-
      CALL HBOOK1(1,'PTFFF    $',100,0.,200.)
C
      CALL HBOOK1(11,'TOTAL ET  $',100,0.,500.)
      CALL HBOOK1(15,'ET ELECTR.$',100,0.,100.)
      CALL HBOOK1(21,'ET JET    $',100,0.,100.)
      CALL HBOOK1(22,'Y  JET    $',100,-10.,10.)
C-
C-----------------------------------------------------------------------
C
C          PROVIDE AUTOMATIC SCALING FOR SCATTER PLOTS.
      CALL HSCALE(0,0.)
C
C          CREATE COPIES OF HISTOGRAMS WITH ID2=ID1+NHSHFT
      DO 100 I=1,MXHIST
      IF(.NOT.HEXIST(I)) GO TO 100
      CALL HCOPY(I,I+NHSHFT,' ')
100   CONTINUE
C
      RETURN
      END
