*
* $Id: hqsig.F,v 1.1.1.1 1996/01/16 17:08:05 mclareni Exp $
*
* $Log: hqsig.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:05  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/03 13/01/93  15.32.49  by  John Allison
*-- Author :
      SUBROUTINE HQSIG (IEDGE, IHQERR)
      INTEGER IEDGE, IHQERR
* Finds significant points which will become centres of the radial basis
*   functions.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcunit.inc"
 
      CHARACTER*80 CHQMES
 
      IHQERR = 0
 
      IF (NDIM .EQ. 1) THEN
         NDIMC = 1
         NDIMWH (1) = 1
         CALL HQSIG1 (NX, 1. / NX, IEDGE, IHQERR)
         IF (IHQERR .NE. 0) GO TO 50
      ELSE IF (NDIM .EQ. 2) THEN
         NDIMC = 2
         NDIMWH (1) = 1
         NDIMWH (2) = 2
         CALL HQSIG2 (NX, NY, 1. / (NX * NY), IEDGE, IHQERR)
         IF (IHQERR .NE. 0) GO TO 50
      ELSE IF (NDIM .EQ. 3) THEN
         NDIMC = 3
         NDIMWH (1) = 1
         NDIMWH (2) = 2
         NDIMWH (3) = 3
         CALL HQSIG3 (NX, NY, NZ, 1. / (NX * NY * NZ),
     +   IEDGE, IHQERR)
         IF (IHQERR .NE. 0) GO TO 50
      ELSE
         GO TO 10
      END IF
 
      IF (NSIG .LT. NSMIN) GO TO 20
      IF (NSIG .GT. NBTOT) GO TO 30
 
      GO TO 50
 
   10 CONTINUE
      WRITE (CHQMES, '(I3, '' dimensions not programmed yet.'')') NDIM
      IHQERR = 10
      GO TO 40
   20 CONTINUE
      WRITE (CHQMES, '(''Only'', I5,
     +'' significant points found - minimum'', I3)') NSIG, NSMIN
      CALL HBUG (CHQMES, 'HQSIG', IDMQ)
      WRITE (LOUT,10000)
      IHQERR = 20
      GO TO 50
   30 CONTINUE
      WRITE (CHQMES, '(I5, '' significant '
     +//'points found - near to no. of bins ('', I5, '').'')')
     +NSIG, NBTOT
      IHQERR = 30
      GO TO 40
 
   40 CONTINUE
      CALL HBUG (CHQMES, 'HQSIG', IDMQ)
 
   50 CONTINUE
 
10000 FORMAT (
     +3X, 'Data of insufficient statistical significance and/or'/
     +3X, 'errors overestimated and/or SENSIT parameter too small?')
 
      END
