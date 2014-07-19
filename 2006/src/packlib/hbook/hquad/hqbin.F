*
* $Id: hqbin.F,v 1.2 1997/09/18 07:24:23 couet Exp $
*
* $Log: hqbin.F,v $
* Revision 1.2  1997/09/18 07:24:23  couet
* - Bug fixed by Pierre Astier <astier@lpnax1.in2p3.fr>
*
*   " I experienced a crash (floating invalid) in hqpois called by hquad when i
*     running on a large ntuple. This was due to a division by zero which should
*     not happen given the preprocessing done by hqbin. hqbin takes copies of
*     links belonging to a link area (l1v,l1h,...) into local variables (lv,lh).
*     Then in case of an ntuple, it scans the tuple. Finally it does some
*     postprocessing using lv and lh to index the data. My crash was due to the
*     fact that the tuple scan is causing some bank displacements, which are
*     reflected in l1v,l1h ... but not in their local copies. As far as I know
*     this is a very common trap when using zebra. I fixed the problem and my
*     crash disappeared."
*
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/03 13/01/93  12.14.18  by  John Allison
*-- Author :    John Allison   12/09/92
      SUBROUTINE HQBIN (NN, IVMODE, IHQERR)
      INTEGER NN (*), IVMODE, IHQERR
* Makes the MQ histogram in L1H or L2H etc.:
*   (a) from HBOOK histogram or
*   (b) MQ "eventogram".
*
* NN is a vector of nos. of bins in each coordinate (normally equal to NX etc.).
*  (Note: (a) NN must be compatible with the HBOOK histogram or
*         (b) its elements must be less than or equal to those notified to
*               HQINIT at initialisation.)
* IVMODE = 0, do NOT set variance of empty bins (i.e., for HQSIG etc.).
*        = 1, set variance of empty bins equal to square of mean variance, i.e.,
*               the variance it should have in the "extreme gaussian
*               approximation", for the purpose of least squares fitting.
* IHQERR = 0 if all's OK.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
 
      CHARACTER*80 CHQMES
      INTEGER NNX, NNY, NNZ, NNBTOT
      INTEGER I, IX, IY, IZ, L, LH, LV
      INTEGER NOUT
      REAL H, WEIGHT
* Note: V has to be big enough to accept all variables, but only 1st 3 are used.
*   (No. is checked in HQUAD.)
      REAL V (NSMAX), X, Y, Z
      EQUIVALENCE (X, V(1)), (Y, V(2)), (Z, V(3))
 
      IHQERR = 0
 
      IF (NDIM .EQ. 1) THEN
         NNX = NN (1)
         NNBTOT = NNX
         LH = L1H
         LV = L1V
      ELSE IF (NDIM .EQ. 2) THEN
         NNX = NN (1)
         NNY = NN (2)
         NNBTOT = NNX * NNY
         LH = L2H
         LV = L2V
      ELSE IF (NDIM .EQ. 3) THEN
         NNX = NN (1)
         NNY = NN (2)
         NNZ = NN (3)
         NNBTOT = NNX * NNY * NNZ
         LH = L3H
         LV = L3V
      ELSE
         GO TO 80
      END IF
 
      IF (NTUPLE) THEN
         CALL HGNPAR (IDMQ, 'HQBIN')
         IF (LCIDN .LE. 0) GO TO 90
         CALL UZERO (Q, LH + 1, LH + NNBTOT)
         CALL UZERO (Q, LV + 1, LV + NNBTOT)
         NOUT = 0
         IF (NDIM .EQ. 1) THEN
            DO 10    I = 1, NMQEVS
               CALL HGNF (IDMQ, I, V, IHQERR)
               IF (IHQERR .NE. 0) GO TO 100
               IF (X .LT. XMI .OR. X .GT. XMA) THEN
                  NOUT = NOUT + 1
               ELSE
                  IX = NNX * (X - XMI) / DXT
                  IF (IX .GE. NNX) IX = NNX - 1
                  L = IX + 1
                  WEIGHT = 1.
                  Q (L1H + L) = Q (L1H + L) + WEIGHT
                  Q (L1V + L) = Q (L1V + L) + WEIGHT ** 2
               END IF
   10       CONTINUE
         ELSE IF (NDIM .EQ. 2) THEN
            DO 20    I = 1, NMQEVS
               CALL HGNF (IDMQ, I, V, IHQERR)
               IF (IHQERR .NE. 0) GO TO 100
               IF (X .LT. XMI .OR. X .GT. XMA .OR.
     +         Y .LT. YMI .OR. Y .GT. YMA) THEN
                  NOUT = NOUT + 1
               ELSE
                  IX = NNX * (X - XMI) / DXT
                  IY = NNY * (Y - YMI) / DYT
                  IF (IX .GE. NNX) IX = NNX - 1
                  IF (IY .GE. NNY) IY = NNY - 1
                  L = NNX * IY + IX + 1
                  WEIGHT = 1.
                  Q (L2H + L) = Q (L2H + L) + WEIGHT
                  Q (L2V + L) = Q (L2V + L) + WEIGHT ** 2
               END IF
   20       CONTINUE
         ELSE IF (NDIM .EQ. 3) THEN
            DO 30    I = 1, NMQEVS
               CALL HGNF (IDMQ, I, V, IHQERR)
               IF (IHQERR .NE. 0) GO TO 100
               IF (X .LT. XMI .OR. X .GT. XMA .OR.
     +         Y .LT. YMI .OR. Y .GT. YMA .OR.
     +         Z .LT. ZMI .OR. Z .GT. ZMA) THEN
                  NOUT = NOUT + 1
               ELSE
                  IX = NNX * (X - XMI) / DXT
                  IY = NNY * (Y - YMI) / DYT
                  IZ = NNZ * (Z - ZMI) / DZT
                  IF (IX .GE. NNX) IX = NNX - 1
                  IF (IY .GE. NNY) IY = NNY - 1
                  IF (IZ .GE. NNZ) IZ = NNZ - 1
                  L = NNX * NNY * IZ + NNX * IY + IX + 1
                  WEIGHT = 1.
                  Q (L3H + L) = Q (L3H + L) + WEIGHT
                  Q (L3V + L) = Q (L3V + L) + WEIGHT ** 2
               END IF
   30       CONTINUE
         ELSE
            GO TO 80
         END IF
         IF (NOUT .GT. 0) WRITE (LOUT, 10000)  NOUT
      ELSE
         CALL HUNPAK (IDMQ, Q (LH + 1), 'HIST', 0)
         CALL HUNPKE (IDMQ, Q (LV + 1), 'HIST', 0)
* Note: although it says ' ' is equivalent to 'HIST' for HUNPAK, it isn't
*   for the undocumented routine HUNPKE.  For the latter, to get stored
*   errors, one must specify 'HIST'.
         DO 40    L = 1, NNBTOT
            Q (LV + L) = VSCALE * Q (LV + L) ** 2
   40    CONTINUE
      END IF
*
* Bug fixed by: Pierre Astier <astier@lpnax1.in2p3.fr>
*
* HGNF may have changed l{1,2,3}{V,H} for large ntuples,
* copy LV, LH once again.
*
* >>>>
      IF (NDIM .EQ. 1) THEN
         LH = L1H
         LV = L1V
      ELSE IF (NDIM .EQ. 2) THEN
         LH = L2H
         LV = L2V
      ELSE IF (NDIM .EQ. 3) THEN
         LH = L3H
         LV = L3V
      END IF
* <<<<
 
* Find HQMIN, HQMAX and NMQFUL.
      HQMIN =  1.E20
      HQMAX = -1.E20
      NMQFUL = 0
      DO 50    L = 1, NNBTOT
         H = Q (LH + L)
         IF (H .NE. 0.) NMQFUL = NMQFUL + 1
         IF (HQMIN .GT. H) HQMIN = H
         IF (HQMAX .LT. H) HQMAX = H
   50 CONTINUE
 
* Compute mean variance.
      WMQEVS = 0.
      VMEAN1 = 0.
      DO 60    L = 1, NNBTOT
         WMQEVS = WMQEVS + Q (LH + L)
         VMEAN1 = VMEAN1 + Q (LV + L)
   60 CONTINUE
      IF (WMQEVS .EQ. 0.) GO TO 110
      VMEAN1 = VMEAN1 / WMQEVS
      VMEAN2 = VMEAN1 ** 2
 
* Set variances of empty bins to VMEAN2 if requested.
      IF (IVMODE .EQ. 1) THEN
         DO 70    L = 1, NNBTOT
            IF (Q (LV + L) .EQ. 0.) Q (LV + L) = VMEAN2
   70    CONTINUE
      END IF
 
      GO TO 130
 
   80 CONTINUE
      WRITE (CHQMES, '(I3, '' dimensions not programmed yet.'')') NDIM
      IHQERR = 1
      GO TO 120
   90 CONTINUE
      WRITE (CHQMES, '(''Invalid link'', I10, '' from HGNPAR.'')')
     +LCIDN
      GO TO 120
  100 CONTINUE
      WRITE (CHQMES, '(''Error'', I3, '' in HGNF.'')') IHQERR
      GO TO 120
  110 CONTINUE
      CHQMES = 'No events.'
      IHQERR = 2
      GO TO 120
 
  120 CONTINUE
      CALL HBUG (CHQMES, 'HQBIN', IDMQ)
 
  130 CONTINUE
 
10000 FORMAT (1X, 'HQBIN: Warning -', I10, ' events outside range.')
 
      END
