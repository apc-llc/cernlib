*
* $Id: hqsig3.F,v 1.1.1.1 1996/01/16 17:08:05 mclareni Exp $
*
* $Log: hqsig3.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:05  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/13 14/05/93  09.08.25  by  John Allison
*-- Author :
      SUBROUTINE HQSIG3 (NX0, NY0, NZ0, VOLB0, IEDGE, IHQERR)
      INTEGER NX0, NY0, NZ0, IEDGE, IHQERR
      REAL VOLB0
* Finds significant points which will become centres of the radial basis
*   functions for 3-D histograms.
*
* Histogram contents and variances are in Q (L3H +1,...) and Q (L3V + 1,...).
***** Note: this routine overwrites histogram contents and variances.
*
* NX0    =  number of x bins.
* NY0    =  number of y bins.
* NZ0    =  number of z bins.
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
* This routine must be called with NDIMC (the current no. of dimensions) = 3.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
 
* Miscellaneous local variables.
      LOGICAL ANY, THIS, EDGED
      INTEGER NFACTR
      INTEGER NBTOT1, NBINS1 (NDMAX), NX1, NY1, NZ1, NSLRMS
      EQUIVALENCE (NX1, NBINS1 (1)), (NY1, NBINS1 (2)),
     +(NZ1, NBINS1 (3))
      INTEGER IDIM, NX00, NY00
      INTEGER L, LF, LJ
      INTEGER L1M, L1P, L2M, L2P, l2PP,L3M, L3P, L3PP, L3PPP, L3PPPP
      INTEGER IX, IY, IZ
      INTEGER NDIMW2 (NDMAX)
      INTEGER IXE, IYE, IZE, IEDX (2), IEDY (2), IEDZ (2)
      REAL DXN1, DYN1, DZN1, VOLB1
      REAL AL, SL, V, V1M, V1P, V2M, V2P, V3M, V3P, VL, DELTA
      REAL AS, VS, SS
      REAL DEDGE, EDGEN (2)
      REAL FACTOR
 
      DATA EDGEN /0., 1./
 
* Check NDIMC
      IF (NDIMC .NE. 3) GO TO 410
 
* Clear parameters.
      IEDGE  = 0
      IHQERR = 0
 
* Set edge parameter - edge points this many bins from edge.
      IF (NDIMC .LT. NDIM) THEN
         DEDGE = 0.5
      ELSE
         DEDGE = 0.
      END IF
 
* DXN1, etc. and NBTOT1 are current values of bin sizes and total no. of bins.
      NX1    = NX0
      NY1    = NY0
      NZ1    = NZ0
      DXN1   = 1. / NX1
      DYN1   = 1. / NY1
      DZN1   = 1. / NZ1
      NBTOT1 = NX1 * NY1 * NZ1
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
*   Relax by factor 0.5 for 3-D.
      SLCUT = 0.5 * 1.2 * SQRT (ALOG (FLOAT (NBTOT1))) / SENSIT
 
* Record whether any significant points have been found THIS loop.
      THIS = .FALSE.
 
* A convention here is that the pointers for the 6 points surrounding the point
*   of interest (whose pointer is L) are:
*   L1M, L1P for 1st dimension Minus and Plus one bin.
*   L2M, L2P for 2nd dimension Minus and Plus one bin.
*   L3M, L3P for 3rd dimension Minus and Plus one bin.
      DELTA = MIN (DXN1, DYN1, DZN1)
      SLRMS = 0.
      NSLRMS = 0
      DO 40 IX = 2, NX1 - 1
         XBIN = (IX - 0.5) * DXN1
         DO 30 IY = 2, NY1 - 1
            YBIN = (IY - 0.5) * DYN1
            DO 20 IZ = 2, NZ1 - 1
               ZBIN = (IZ - 0.5) * DZN1
               L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
               L1M = L - 1
               L1P = L + 1
               L2M = L - NX1
               L2P = L + NX1
               L3M = L - NX1 * NY1
               L3P = L + NX1 * NY1
               AL = Q (L3H + L1M) + Q (L3H + L1P) + Q (L3H + L2M) + Q
     +         (L3H + L2P) + Q (L3H + L3M) + Q (L3H + L3P) - 6. * Q
     +         (L3H + L)
               V = Q (L3V + L)
               V1M = Q (L3V + L1M)
               V1P = Q (L3V + L1P)
               V2M = Q (L3V + L2M)
               V2P = Q (L3V + L2P)
               V3M = Q (L3V + L3M)
               V3P = Q (L3V + L3P)
               IF (V   .EQ. 0.) V   = VMEAN2
               IF (V1M .EQ. 0.) V1M = VMEAN2
               IF (V1P .EQ. 0.) V1P = VMEAN2
               IF (V2M .EQ. 0.) V2M = VMEAN2
               IF (V2P .EQ. 0.) V2P = VMEAN2
               IF (V3M .EQ. 0.) V3M = VMEAN2
               IF (V3P .EQ. 0.) V3P = VMEAN2
               VL = V1M + V1P + V2M + V2P + V3M + V3P + 36. * V
               SL = ABS (AL / SQRT (VL))
               IF (SL .GT. 1.E-6) THEN
                  SLRMS = SLRMS + SL ** 2
                  NSLRMS = NSLRMS + 1
               END IF
               IF (SL .GE. SLCUT) THEN
                  CALL HQSIGA (BINV, Q (L3H + L), VOLB1, DELTA, SL,
     +            IHQERR)
                  IF (IHQERR .EQ. 0) THEN
                     ANY = .TRUE.
                     THIS = .TRUE.
                  ELSE IF (IHQERR .LT. 0) THEN
                     IHQERR = 0
                  ELSE
                     GO TO 420
                  END IF
               END IF
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
      SLRMS = SQRT (SLRMS / NSLRMS)
 
* If this is the first time any significant points have been found...
      IF ((ANY .AND. .NOT. EDGED) .OR. (NDIMC .NE. NDIM)) THEN
         EDGED = .TRUE.
* Record bin size.
         IF (NDIM .EQ. NDIMC) THEN
            NXN = NX1
            NYN = NY1
            NZN = NZ1
         END IF
*************************************************************
* New strategy - add points if slope is significant.
* Low x...
         IX = 1
         XBIN = 0.
         DO 60 IY = 1, NY1
            YBIN = (IY - 0.5) * DYN1
            DO 50 IZ = 1, NZ1
               ZBIN = (IZ - 0.5) * DZN1
               L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
               L1P = L + 1
               AS  = Q (L3H + L1P) - Q (L3H + L)
               V   = Q (L3V + L)
               V1P = Q (L3V + L1P)
               IF (V   .EQ. 0.) V   = VMEAN2
               IF (V1P .EQ. 0.) V1P = VMEAN2
               VS = V1P + V
               SS = ABS (AS / SQRT (VS))
               IF (SS .GE. SLCUT) THEN
                  CALL HQSIGA (BINV, Q (L3H + L), VOLB1, DELTA,
     +            SS, IHQERR)
                  IF (IHQERR .EQ. 0) THEN
                     ANY = .TRUE.
                     THIS = .TRUE.
                  ELSE IF (IHQERR .LT. 0) THEN
                     IHQERR = 0
                  ELSE
                     GO TO 420
                  END IF
               END IF
   50       CONTINUE
   60    CONTINUE
* High x...
         IX = NX1
         XBIN = 1.
         DO 80 IY = 1, NY1
            YBIN = (IY - 0.5) * DYN1
            DO 70 IZ = 1, NZ1
               ZBIN = (IZ - 0.5) * DZN1
               L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
               L1M = L - 1
               AS  = Q (L3H + L1M) - Q (L3H + L)
               V   = Q (L3V + L)
               V1M = Q (L3V + L1M)
               IF (V   .EQ. 0.) V   = VMEAN2
               IF (V1M .EQ. 0.) V1M = VMEAN2
               VS = V1M + V
               SS = ABS (AS / SQRT (VS))
               IF (SS .GE. SLCUT) THEN
                  CALL HQSIGA (BINV, Q (L3H + L), VOLB1, DELTA,
     +            SS, IHQERR)
                  IF (IHQERR .EQ. 0) THEN
                     ANY = .TRUE.
                     THIS = .TRUE.
                  ELSE IF (IHQERR .LT. 0) THEN
                     IHQERR = 0
                  ELSE
                     GO TO 420
                  END IF
               END IF
   70       CONTINUE
   80    CONTINUE
* Low y...
         IY = 1
         YBIN = 0.
         DO 100 IX = 1, NX1
            XBIN = (IX - 0.5) * DXN1
            DO 90 IZ = 1, NZ1
               ZBIN = (IZ - 0.5) * DZN1
               L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
               L2P = L + NX1
               AS  = Q (L3H + L2P) - Q (L3H + L)
               V   = Q (L3V + L)
               V2P = Q (L3V + L2P)
               IF (V   .EQ. 0.) V   = VMEAN2
               IF (V2P .EQ. 0.) V2P = VMEAN2
               VS = V2P + V
               SS = ABS (AS / SQRT (VS))
               IF (SS .GE. SLCUT) THEN
                  CALL HQSIGA (BINV, Q (L3H + L), VOLB1, DELTA,
     +            SS, IHQERR)
                  IF (IHQERR .EQ. 0) THEN
                     ANY = .TRUE.
                     THIS = .TRUE.
                  ELSE IF (IHQERR .LT. 0) THEN
                     IHQERR = 0
                  ELSE
                     GO TO 420
                  END IF
               END IF
   90       CONTINUE
  100    CONTINUE
* High y...
         IY = NY1
         YBIN = 1.
         DO 120 IX = 1, NX1
            XBIN = (IX - 0.5) * DXN1
            DO 110 IZ = 1, NZ1
               ZBIN = (IZ - 0.5) * DZN1
               L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
               L2M = L - NX1
               AS  = Q (L3H + L2M) - Q (L3H + L)
               V   = Q (L3V + L)
               V2M = Q (L3V + L2M)
               IF (V   .EQ. 0.) V   = VMEAN2
               IF (V2M .EQ. 0.) V2M = VMEAN2
               VS = V2M + V
               SS = ABS (AS / SQRT (VS))
               IF (SS .GE. SLCUT) THEN
                  CALL HQSIGA (BINV, Q (L3H + L), VOLB1, DELTA,
     +            SS, IHQERR)
                  IF (IHQERR .EQ. 0) THEN
                     ANY = .TRUE.
                     THIS = .TRUE.
                  ELSE IF (IHQERR .LT. 0) THEN
                     IHQERR = 0
                  ELSE
                     GO TO 420
                  END IF
              END IF
  110       CONTINUE
  120    CONTINUE
* Low z...
         IZ = 1
         ZBIN = 0.
         DO 140 IX = 1, NX1
            XBIN = (IX - 0.5) * DXN1
            DO 130 IY = 1, NY1
               YBIN = (IY - 0.5) * DYN1
               L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
               L3P = L + NX1 * NY1
               AS  = Q (L3H + L3P) - Q (L3H + L)
               V   = Q (L3V + L)
               V3P = Q (L3V + L3P)
               IF (V   .EQ. 0.) V   = VMEAN2
               IF (V3P .EQ. 0.) V3P = VMEAN2
               VS = V3P + V
               SS = ABS (AS / SQRT (VS))
               IF (SS .GE. SLCUT) THEN
                  CALL HQSIGA (BINV, Q (L3H + L), VOLB1, DELTA,
     +            SS, IHQERR)
                  IF (IHQERR .EQ. 0) THEN
                     ANY = .TRUE.
                     THIS = .TRUE.
                  ELSE IF (IHQERR .LT. 0) THEN
                     IHQERR = 0
                  ELSE
                     GO TO 420
                  END IF
               END IF
  130       CONTINUE
  140    CONTINUE
* High z...
         IZ = NZ1
         ZBIN = 1.
         DO 160 IX = 1, NX1
            XBIN = (IX - 0.5) * DXN1
            DO 150 IY = 1, NY1
               YBIN = (IY - 0.5) * DYN1
               L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
               L3M = L - NX1 * NY1
               AS = Q (L3H + L3M) - Q (L3H + L)
               V  = Q (L3V + L)
               V3M = Q (L3V + L3M)
               IF (V   .EQ. 0.) V   = VMEAN2
               IF (V3M .EQ. 0.) V3M = VMEAN2
               VS = V3M + V
               SS = ABS (AS / SQRT (VS))
               IF (SS .GE. SLCUT) THEN
                  CALL HQSIGA (BINV, Q (L3H + L), VOLB1, DELTA,
     +            SS, IHQERR)
                  IF (IHQERR .EQ. 0) THEN
                     ANY = .TRUE.
                     THIS = .TRUE.
                  ELSE IF (IHQERR .LT. 0) THEN
                     IHQERR = 0
                  ELSE
                     GO TO 420
                  END IF
              END IF
  150       CONTINUE
  160    CONTINUE
* End of new strategy.
*************************************************************************
******************************************************************
* Note: the following has been modified by commenting out some statements
*   to suppress calls to other HQSIGn routines.  This also has the effect
*   of to forcing the adding of corner points.
******************************************************************
* Now we are going to present HQSIG2 with the faces of this 3-D histogram.
*   HQSIG2 will think it's a 2-D histogram.  When it comes to adding a point,
*   however, HQSIGA must know:
*     (a) the original dimensionality, NDIM,
*     (b) the current dimensionality, NDIMC,
*     (c) the coordinate(s) being treated, (NDIMWH (I), I = 1, NDIMC) and
*     (d) the value of the remaining coordinates,
*           (BINV (I), I = NDIMC + 1, NDIM), if any, destined for
*           (SIGV (NSIG, (NDIMWH(I))), I = NDIMC + 1, NDIM).
* Preserve current values of NDIMWH (NDIMC is assumed to be 3 at this point).
         DO 170   IDIM = 1, NDIMC
            NDIMW2 (IDIM) = NDIMWH (IDIM)
  170    CONTINUE
* Find sig. points along the 6 faces...
         NDIMC = 2
* x faces...
         DO 200   IX = 1, NX1, NX1 - 1
            IXE = IX / NX1 + 1
            DO 190   IY = 1, NY1
               DO 180   IZ = 1, NZ1
                  L  = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
                  LF = (IZ - 1) * NY1 + IY
                  Q (L2H + LF) = Q (L3H + L)
                  Q (L2V + LF) = Q (L3V + L)
  180          CONTINUE
  190       CONTINUE
            NDIMWH (1) = NDIMW2 (2)
            NDIMWH (2) = NDIMW2 (3)
            NDIMWH (3) = NDIMW2 (1)
            BINV (3) = EDGEN (IXE) -
     +      (2. * EDGEN (IXE) - 1.) * DEDGE * DXN1
            IEDX (IXE) = 0
************         CALL HQSIG2 (NY1, NZ1, VOLB1, IEDX (IXE), IHQERR)
            IF (IHQERR .NE. 0) GO TO 420
  200    CONTINUE
* y faces...
         DO 230   IY = 1, NY1, NY1 - 1
            IYE = IY / NY1 + 1
            DO 220   IX = 1, NX1
               DO 210   IZ = 1, NZ1
                  L  = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
                  LF = (IZ - 1) * NX1 + IX
                  Q (L2H + LF) = Q (L3H + L)
                  Q (L2V + LF) = Q (L3V + L)
  210          CONTINUE
  220       CONTINUE
            NDIMWH (1) = NDIMW2 (1)
            NDIMWH (2) = NDIMW2 (3)
            NDIMWH (3) = NDIMW2 (2)
            BINV (3) = EDGEN (IYE) -
     +      (2. * EDGEN (IYE) - 1.) * DEDGE * DYN1
            IEDY (IYE) = 0
************            CALL HQSIG2 (NX1, NZ1, VOLB1, IEDY (IYE), IHQERR)
            IF (IHQERR .NE. 0) GO TO 420
  230    CONTINUE
* z faces...
         DO 260   IZ = 1, NZ1, NZ1 - 1
            IZE = IZ / NZ1 + 1
            DO 250   IX = 1, NX1
               DO 240   IY = 1, NY1
                  L  = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
                  LF = (IY - 1) * NX1 + IX
                  Q (L2H + LF) = Q (L3H + L)
                  Q (L2V + LF) = Q (L3V + L)
  240          CONTINUE
  250       CONTINUE
            NDIMWH (1) = NDIMW2 (1)
            NDIMWH (2) = NDIMW2 (2)
            NDIMWH (3) = NDIMW2 (3)
            BINV (3) = EDGEN (IZE) -
     +      (2. * EDGEN (IZE) - 1.) * DEDGE * DZN1
            IEDZ (IZE) = 0
************            CALL HQSIG2 (NX1, NY1, VOLB1, IEDZ (IZE), IHQERR)
            IF (IHQERR .NE. 0) GO TO 420
  260    CONTINUE
* Restore current values of NDIMWH.
         NDIMC = 3
         DO 270   IDIM = 1, NDIMC
            NDIMWH (IDIM) = NDIMW2 (IDIM)
  270    CONTINUE
* Add corner points if adjacent faces have points.
         DO 300   IX = 1, NX1, NX1 - 1
            IXE = IX / NX1 + 1
            DO 290   IY = 1, NY1, NY1 - 1
               IYE = IY / NY1 + 1
               DO 280   IZ = 1, NZ1, NZ1 - 1
                  IZE = IZ / NZ1 + 1
                  IF (IEDX (IXE) .GE. 0 .AND. IEDY (IYE) .GE. 0 .AND.
     +            IEDZ (IZE) .GE. 0) THEN
                     ANY = .TRUE.
                     XBIN = EDGEN (IXE) - (2. * EDGEN (IXE) - 1.) *
     +               DEDGE * DXN1
                     YBIN = EDGEN (IYE) - (2. * EDGEN (IYE) - 1.) *
     +               DEDGE * DYN1
                     ZBIN = EDGEN (IZE) - (2. * EDGEN (IZE) - 1.) *
     +               DEDGE * DZN1
                     L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
                     CALL HQSIGA (BINV, Q (L3H + L), VOLB1, SMALLD,
     +               SLCUT, IHQERR)
                     IF (IHQERR .EQ. 0) THEN
                        ANY = .TRUE.
                        THIS = .TRUE.
                     ELSE IF (IHQERR .LT. 0) THEN
                        IHQERR = 0
                     ELSE
                        GO TO 420
                     END IF
                  END IF
  280          CONTINUE
  290       CONTINUE
  300    CONTINUE
* Are the extremities zero?
         DO 330   IX = 1, NX1, NX1 - 1
            DO 320   IY = 1, NY1, NY1 - 1
               DO 310   IZ = 1, NZ1, NZ1 - 1
                  L = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 + IX
                  IF (Q (L3H + L) .NE. 0) GO TO 370
  310          CONTINUE
  320       CONTINUE
  330    CONTINUE
         DO 340   IXE = 1, 2
            IF (IEDX (IXE) .GE. 0) GO TO 370
  340    CONTINUE
         DO 350   IYE = 1, 2
            IF (IEDY (IYE) .GE. 0) GO TO 370
  350    CONTINUE
         DO 360   IZE = 1, 2
            IF (IEDZ (IZE) .GE. 0) GO TO 370
  360    CONTINUE
* If so, mark for zero asyptotic constrained fit.
         IEDGE = 2
  370    CONTINUE
      END IF
 
* If no points have been found, or points have been found this time round (i.e.,
*   cease searching if points have been found but not this time round).
**** (Not sure about the wisdom of this (why did I introduce it?) so disable for
****   now.)
*     IF (.NOT. ANY .OR. THIS) THEN
      IF (NTUPLE) THEN
* Rebin - the data is from an ntuple.  This simplifies things immensely.
* Double significance by increasing bin size by cube root of 2.
         FACTOR = 1.3
         NFACTR = 10. * FACTOR
         IF (NX1 .GE. NFACTR .AND. NY1 .GE. NFACTR .AND. NZ1 .GE.
     +   NFACTR) THEN
            NX1 = NX1 / FACTOR
            NY1 = NY1 / FACTOR
            NZ1 = NZ1 / FACTOR
            NBTOT1 = NX1 * NY1 * NZ1
            DXN1 = 1. / NX1
            DYN1 = 1. / NY1
            DZN1 = 1. / NZ1
            VOLB1 = DXN1 * DYN1 * DZN1
* Get contents and errors.
            CALL HQBIN (NBINS1, 0, IHQERR)
            IF (IHQERR. NE. 0) GO TO 420
            GO TO 10
         END IF
      ELSE
* Rebin - double up.
***** NOTE: bin doubling leaves the odd bin over.  Not knowing what to do with
*****   it I simply leave it - possibly a significant point might be missed but
*****   the above code gave it at least one chance.  When it comes to choosing
*****   bins for finding significant points for N-tuples, choose 2**N bins.
         IF (NX1 .GE. 20 .AND. NY1 .GE. 20 .AND. NZ1 .GE. 20) THEN
            NX00 = NX1
            NY00 = NY1
            NX1 = NX1 / 2
            NY1 = NY1 / 2
            NZ1 = NZ1 / 2
            DXN1 = 2. * DXN1
            DYN1 = 2. * DYN1
            DZN1 = 2. * DZN1
            VOLB1 = 8. * VOLB1
            NBTOT1 = NX1 * NY1 * NZ1
            DO 400   IZ = 1, NZ1
               DO 390   IY = 1, NY1
                  DO 380   IX = 1, NX1
                     L = 2 * (IZ - 1) * NX00 * NY00 + 2 * (IY - 1) *
     +               NX00 + 2 * (IX - 1) + 1
                     L1P = L + 1
                     L2P = L + NX00
                     L2PP = L + NX00 + 1
                     L3P = L + NX00 * NY00
                     L3PP = L + NX00 * NY00 + 1
                     L3PPP = L + NX00 * NY00 + NX00
                     L3PPPP = L + NX00 * NY00 + NX00 + 1
                     LJ = (IZ - 1) * NX1 * NY1 + (IY - 1) * NX1 +
     +               IX
                     Q (L3H + LJ) = Q (L3H + L) + Q (L3H + L1P) +
     +               Q (L3H + L2P) + Q (L3H + L2PP) + Q (L3H + L3P)
     +               + Q (L3H + L3PP) + Q (L3H + L3PPP) + Q (L3H +
     +               L3PPPP)
                     Q (L3V + LJ) = Q (L3V + L) + Q (L3V + L1P) +
     +               Q (L3V + L2P) + Q (L3V + L2PP) + Q (L3V + L3P)
     +               + Q (L3V + L3PP) + Q (L3V + L3PPP) + Q (L3V +
     +               L3PPPP)
  380             CONTINUE
  390          CONTINUE
  400       CONTINUE
            GO TO 10
         END IF
      END IF
*     END IF
 
* If no points have been found this pass, use IEDGE as a flag.
      IF (.NOT. ANY) IEDGE = -1
 
      GO TO 420
 
* Errors.
  410 CONTINUE
      CALL HBUG ('NDIMC not 3.', 'HQSIG3', IDMQ)
      IHQERR = 10
 
  420 CONTINUE
 
      END
