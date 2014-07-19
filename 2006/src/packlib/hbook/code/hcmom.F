*
* $Id: hcmom.F,v 1.1.1.1 1996/01/16 17:07:33 mclareni Exp $
*
* $Log: hcmom.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:33  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HCMOM(IDD,CMOM)
*.==========>
*.  CALCULATES INITIAL VALUES OF THE CHEBYSHEF COEFFICIENTS
*.  A1,A2,...,AN IN HPOLYN AS THE SUM OF CONTENTS/NCHAN, MEAN, RMS
*.  AND THIRD (UP TO THE 6-TH) CENTRAL MOMENTS OF HISTOGRAM IDD.
*.
*.           OUTPUT ARGUMENT  CMOM    = ARRAY OF CMOMENTS
*..=========> ( E.Lessner )
#include "hbook/hcfit2.inc"
#include "hbook/hcfit3.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfitr.inc"
      DIMENSION XVAL(6), CMOM(*)
*
      DO 10 I=1,7
         CMOM(I)=0.
   10 CONTINUE
      DO 15 I=1,6
         XVAL(I)=0.
   15 CONTINUE
*
      IF (ALLCHA.EQ.0.) GO TO 99
      NCHAN=IQ(LPRX)
      NCHANE=0
      X=XMIN-BINWID*0.5
      DO 30 I=1,NCHAN
         IF(IFTRNG.NE.0)THEN
            IF(I.LT.IFXLOW.OR.I.GT.IFXUP)GO TO 30
         ENDIF
         X=X+BINWID
         NCHANE=NCHANE+1
         W=HCX(I,1)
         W=ABS(W)
         DO 25 J=1,6
            W=W*X
            XVAL(J)=XVAL(J)+W
   25    CONTINUE
   30 CONTINUE
*
      CMOM(1)=ALLCHA/NCHANE
      IF (NPOLYN.EQ.0) GO TO 99
      CMOM(2)=HSTATI(IDD,1,'HIST',1)
      IF (NPOLYN.EQ.1) GO TO 99
      CMOM(3)=HSTATI(IDD,2,'HIST',1)
      IF (NPOLYN.EQ.2) GO TO 99
*
      CMOM(4)=(XVAL(3) - 3.*CMOM(2)*XVAL(2))/ALLCHA +
     +  2.*CMOM(2)**3
      IF (NPOLYN.EQ.3) GO TO 99
*
      CMOM(5)=(XVAL(4) - 4.*CMOM(2)*XVAL(3) + 6.*CMOM(2)*CMOM(2)
     + *XVAL(2))/ALLCHA - 3.*CMOM(2)**4
      IF (NPOLYN.EQ.4) GO TO 99
*
      CMOM(6)=(XVAL(5) - 5.*XVAL(4)*CMOM(2) + 10.*XVAL(3)*
     +  CMOM(2)*CMOM(2) - 10.*XVAL(2)*CMOM(2)**3)/ALLCHA +
     +  4.*CMOM(2)**5
      IF (NPOLYN.EQ.5) GO TO 99
*
      CMOM(7)=( XVAL(6) - 6.*XVAL(5)*CMOM(2) +
     +  15.*XVAL(4)*CMOM(2)*CMOM(2) -
     +  20.*XVAL(3)*CMOM(2)**3 + 15.*XVAL(2)*CMOM(2)**4)/ALLCHA -
     +  5.*CMOM(2)**6
   99 END
