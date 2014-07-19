*
* $Id: hqsig1.F,v 1.1.1.1 1996/01/16 17:08:05 mclareni Exp $
*
* $Log: hqsig1.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:05  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/12 13/05/93  19.32.11  by  John Allison
*-- Author :    John Allison   12/09/92
      SUBROUTINE HQSIG1 (NX0, VOLB0, IEDGE, IHQERR)
      INTEGER NX0, IEDGE, IHQERR
      REAL VOLB0
* Finds significant points which will become centres of the radial basis
*   functions for 1-D histogram.
*
* Histogram contents and variances are in Q (L1H +1,...) and Q (L1V + 1,...).
***** Note: this routine overwrites histogram contents and variances.
*
* NX0    =  number of bins.
* VOLB0  =  volume of bins (normalised coordinate space).
* IEDGE  =  0, normal.
*        =  2, zero extremities (i.e., constrained fit recommended).
*        = <0, special case - no significant points found this pass.
* IHQERR =  0 if all's OK.
*
* We want significant points where the rate of change of slope,
*   or in N-dimensions, the N-dimensional Laplacian, is statistically
*   significant.
*
* The coordinates of significant points - stored in HCQCOM - are in a
*   "normalised" system in which the plot variables are mapped to the
*    range 0 -> 1.
*
* Note: NDIMC, the "current" no. of dimensions, must always be 1 when entering
*   this routine.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
 
* Miscellaneous local variables.
      LOGICAL ANY, THIS, EDGED
      INTEGER NBTOT1, NX1, NSLRMS
      INTEGER IX
      INTEGER L, L1M, L1P, LJ
      REAL DXN1, VOLB1
      REAL AL, SL, V, V1M, V1P, VL, DELTA
      REAL DEDGE
 
* Check NDIMC
      IF (NDIMC .NE. 1) GO TO 40
 
* Clear parameters.
      IEDGE  = 0
      IHQERR = 0
 
* Set edge parameter - edge points this many bins from edge.
      IF (NDIMC .LT. NDIM) THEN
         DEDGE = 0.5
      ELSE
         DEDGE = 0.
      END IF
 
* DXN1 and NBTOT1 are current values of bin size and total no. of bins.
      NX1    = NX0
      DXN1   = 1. / NX1
      NBTOT1 = NX1
      VOLB1  = VOLB0
 
* Search histogram to find significant points.  Look for
*   statistically significant values of Laplacian, AL.  When
*   all have been found at this resolution, rebin and look again.
* The first time any have been found, add edge points.
      ANY = .FALSE.
      EDGED = .FALSE.
*   (Loop back here after rebinning.)
   10 CONTINUE
 
* Determine cut so that probability of purely statistical fluctuation is
*   small.  The probability of an x standard deviation fluctuation is
*   approximately exp(-(x/1.2)**2) (a crude fit to data in PDB).  So to
*   make the probability of a stat. fluctn. less than 1 / (no. of bins)
*   (say) (SENSIT is a user factor that adjusts the sensitivity of the cut
*   - normally SENSIT = 1.).
* (Experience shows the above is too strict - relax by extra factor of 0.7.)
      SLCUT = 0.7 * 1.2 * SQRT (ALOG (FLOAT (NBTOT1))) / SENSIT
 
* Record whether any significant points have been found THIS loop.
      THIS = .FALSE.
 
* Scale parameter is bin size in normalised coords.
      DELTA  = DXN1
      NSLRMS = 0
      SLRMS  = 0.
      DO 20    IX = 2, NX1 - 1
* Bin centre on scale 0 -> 1, i.e., normalised coords.
         XBIN = (IX - 0.5) * DXN1
* Pointers...
         L   = IX
         L1M = L - 1
         L1P = L + 1
* Scale parameter is half bin size in normalised coords. (approx.)
         DELTA = DXN1
* Calculate statistical significance of Laplacian, i.e.,
*   finite difference Laplacian / error.
         AL = Q (L1H + L1M) + Q (L1H + L1P) - 2. * Q (L1H + L)
         V  = Q (L1V + L)
         V1M = Q (L1V + L1M)
         V1P = Q (L1V + L1P)
         IF (V   .EQ. 0.) V   = VMEAN2
         IF (V1M .EQ. 0.) V1M = VMEAN2
         IF (V1P .EQ. 0.) V1P = VMEAN2
         VL = V1M + V1P + 4. * V
         SL = ABS (AL / SQRT (VL))
         IF (SL .GT. 1.E-6) THEN
            SLRMS  = SLRMS + SL ** 2
            NSLRMS = NSLRMS + 1
         END IF
* It is significant?
         IF (SL .GE. SLCUT) THEN
* Add significant point.
            CALL HQSIGA (BINV, Q (L1H + L), VOLB1, DELTA, SL, IHQERR)
            IF (IHQERR .EQ. 0) THEN
               ANY  = .TRUE.
               THIS = .TRUE.
            ELSE IF (IHQERR .LT. 0) THEN
               IHQERR = 0
            ELSE
               GO TO 50
            END IF
         END IF
   20 CONTINUE
      SLRMS = SQRT (SLRMS / NSLRMS)
 
* If this is the first time any significant points have been found...
      IF ((ANY .AND. .NOT. EDGED) .OR. (NDIMC .NE. NDIM)) THEN
         EDGED = .TRUE.
* Record bin size.
         IF (NDIM .EQ. NDIMC) THEN
            NXN = NX1
         END IF
******************************************************************
* Note: the following has been modified to force the adding of corner points.
******************************************************************
* Are the extremities zero?
         IF (Q (L1H + 1) .EQ. 0. .AND. Q (L1H + NX1) .EQ. 0.) THEN
* If so, mark for zero asyptotic constrained fit.
            IEDGE = 2
         END IF
***************       IF (IEDGE .NE. 2) THEN
* Add small-DELTA significant points at ends.  ("Small", rather
*   than zero, to avoid 2 sig. points at same location.)
         XBIN = DEDGE * DXN1
         CALL HQSIGA (BINV, Q (L1H + 1), VOLB1, SMALLD, SLCUT, IHQERR)
         IF (IHQERR .EQ. 0) THEN
            ANY = .TRUE.
            THIS = .TRUE.
         ELSE IF (IHQERR .LT. 0) THEN
            IHQERR = 0
         ELSE
            GO TO 50
         END IF
         XBIN = 1.0 - DEDGE * DXN1
         CALL HQSIGA (BINV, Q (L1H + NX1), VOLB1, SMALLD, SLCUT,
     +   IHQERR)
         IF (IHQERR .EQ. 0) THEN
            ANY = .TRUE.
            THIS = .TRUE.
         ELSE IF (IHQERR .LT. 0) THEN
            IHQERR = 0
         ELSE
            GO TO 50
         END IF
***************       END IF
      END IF
 
* If no points have been found, or points have been found this time round (i.e.,
*   cease searching if points have been found but not this time round).
**** (Not sure about the wisdom of this (why did I introduce it?) so disable for
****   now.)
*     IF (.NOT. ANY .OR. THIS) THEN
* Rebin - double up.
***** NOTE: bin doubling leaves the odd bin over.  Not knowing what to do with
*****   it I simply leave it - possibly a significant point might be missed but
*****   the above code gave it at least one chance.  When it comes to choosing
*****   bins for finding significant points for N-tuples, choose 2**N bins.
      IF (NX1 .GE. 20) THEN
         NX1 = NX1 / 2
         DXN1 = 2. * DXN1
         VOLB1 = 2. * VOLB1
         NBTOT1 = NX1
         DO 30    IX = 1, NX1
            L = 2 * (IX - 1) + 1
            L1P = L + 1
            LJ = IX
            Q (L1H + LJ) = Q (L1H + L) + Q (L1H + L1P)
            Q (L1V + LJ) = Q (L1V + L) + Q (L1V + L1P)
   30    CONTINUE
         GO TO 10
      END IF
*     END IF
 
* If no points have been found this pass, use IEDGE as a flag.
      IF (.NOT. ANY) IEDGE = -1
 
      GO TO 50
 
* Errors.
   40 CONTINUE
      CALL HBUG ('NDIMC not 1.', 'HQSIG1', IDMQ)
      IHQERR = 10
 
   50 CONTINUE
 
      END
