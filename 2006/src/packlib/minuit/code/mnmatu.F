*
* $Id: mnmatu.F,v 1.1.1.1 1996/03/07 14:31:30 mclareni Exp $
*
* $Log: mnmatu.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNMATU(KODE)
#include "minuit/d506dp.inc"
CC        prints the covariance matrix v when KODE=1.
CC        always prints the global correlations, and
CC        calculates and prints the individual correlation coefficients
CC
#include "minuit/d506cm.inc"
      DIMENSION VLINE(MNI)
      ISW2 = ISW(2)
      IF (ISW2 .LT. 1)  THEN
          WRITE (ISYSWR,'(1X,A)')  COVMES(ISW2)
          GO TO 500
      ENDIF
      IF (NPAR .EQ. 0)  THEN
          WRITE (ISYSWR,'('' MNMATU: NPAR=0'')')
          GO TO 500
          ENDIF
C                                       . . . . .external error matrix
      IF (KODE .EQ. 1)  THEN
         ISW5 = ISW(5)
         ISW(5) = 2
         CALL MNEMAT(P,MAXINT)
           IF (ISW2.LT.3)  WRITE (ISYSWR,'(1X,A)')  COVMES(ISW2)
         ISW(5) = ISW5
      ENDIF
C                                       . . . . . correlation coeffs. .
      IF (NPAR .LE. 1)   GO TO 500
      CALL MNWERR
C     NCOEF is number of coeff. that fit on one line, not to exceed 20
      NCOEF = (NPAGWD-19)/6
      NCOEF = MIN(NCOEF,20)
      NPARM = MIN(NPAR,NCOEF)
      WRITE (ISYSWR, 150) (NEXOFI(ID),ID=1,NPARM)
  150 FORMAT (/36H PARAMETER  CORRELATION COEFFICIENTS  /
     +         18H       NO.  GLOBAL   ,20I6)
      DO 200 I= 1, NPAR
         IX = NEXOFI(I)
         NDI = I*(I+1)/2
           DO 170 J= 1, NPAR
           M = MAX(I,J)
           N = MIN(I,J)
           NDEX = M*(M-1)/2 + N
           NDJ = J*(J+1)/2
  170      VLINE(J) = VHMAT(NDEX)/SQRT(ABS(VHMAT(NDI)*VHMAT(NDJ)))
         NPARM = MIN(NPAR,NCOEF)
         WRITE (ISYSWR,171)   IX, GLOBCC(I), (VLINE(IT),IT=1,NPARM)
  171    FORMAT (6X,I3,2X,F7.5,1X,20F6.3)
         IF (I.LE.NPARM) GO TO 200
            DO 190 ISO= 1, 10
            NSOFAR = NPARM
            NPARM = MIN(NPAR,NSOFAR+NCOEF)
            WRITE (ISYSWR,181)  (VLINE(IT),IT=NSOFAR+1,NPARM)
  181       FORMAT (19X,20F6.3)
            IF (I .LE. NPARM) GO TO 192
  190       CONTINUE
  192    CONTINUE
  200 CONTINUE
      IF (ISW2.LT.3)  WRITE (ISYSWR,'(1X,A)')  COVMES(ISW2)
  500 RETURN
      END
