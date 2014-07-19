*
* $Id: hqsiga.F,v 1.1.1.1 1996/01/16 17:08:06 mclareni Exp $
*
* $Log: hqsiga.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:06  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.23.42  by  John Allison
*-- Author :
      SUBROUTINE HQSIGA (V, H, VOL, DELTA, SL, IHQERR)
      REAL V (*), H, VOL, DELTA, SL
      INTEGER IHQERR
* Adds significant point at V, bin height H, bin "volume" VOL,
*   scale parameter DELTA. SL is significance in standard deviations.
* Note: VOL is in normalised coordinates, so convert.
* IHQERR = -1 if it's rejected as being too close to existing point.
*        =  0 if all's OK.
* HQSIGA needs:
*     (a) the original dimensionality, NDIM,
*     (b) the current dimensionality, NDIMC,
*     (c) the coordinate(s) being treated, (NDIMWH (I), I = 1, NDIMC) and
*     (d) the value of the remaining coordinates,
*           (BINV (I), I = NDIMC + 1, NDIM), if any, destined for
*           (SIGV (NSIG, (NDIMWH(I))), I = NDIMC + 1, NDIM).
 
#include "hbook/hcqcom.inc"
 
      CHARACTER*80 CHQMES
      INTEGER IDIM, ISIG
 
* SL was introduced so that the scale parameter could be made to depend on
*   the significance of the Laplacian.  However, it was found that if the
*   scale parameters of two nearby multiquadrics differed slightly a beat
*   effect could affect far off regions, or rather, far off regions affected
*   the values of the coefficients.  Thus, this strategy was abandoned so SL
*   is no longer used, but rather than delete it from the argument list here
*   in at all points called, I decided to leave it for possible future use.
*   (To keep CMZ's UNDEFINED command happy there follows a useless statement.)
      IHQERR = SL
      IHQERR = 0
 
* Check it's not too close to an existing point.
      DO 40    ISIG = 1, NSIG
         DO 10    IDIM = 1, NDIMC
            IF (ABS (V (IDIM) - SIGV (ISIG, NDIMWH (IDIM))) .GE.
     +      0.99 * DELTA) GO TO 30
   10    CONTINUE
         DO 20    IDIM = NDIMC + 1, NDIM
            IF (ABS (BINV (IDIM) - SIGV (ISIG, NDIMWH (IDIM))) .GE.
     +      0.99 * DELTA) GO TO 30
   20    CONTINUE
         IHQERR = -1
         GO TO 80
   30    CONTINUE
   40 CONTINUE
 
      NSIG = NSIG + 1
      IF (NSIG .GT. NSMAX) GO TO 70
 
* Record position.
      DO 50    IDIM = 1, NDIMC
         SIGV (NSIG, NDIMWH (IDIM)) = V (IDIM)
   50 CONTINUE
      DO 60    IDIM = NDIMC + 1, NDIM
         SIGV (NSIG, NDIMWH (IDIM)) = BINV (IDIM)
   60 CONTINUE
 
* Record density.
      SIGDEN (NSIG) = H / (VOL * VOLTOT)
 
* Record scale parameter - default is 2 x bin size.
      IF (NDIMC .EQ. NDIM) THEN
         SIGDEL (NSIG) = SPREAD * 2. * DELTA
      ELSE
         SIGDEL (NSIG) = 1.E-6
      END IF
 
      GO TO 80
 
   70 CONTINUE
      WRITE (CHQMES,
     +'(''More than'', I4, '' significant points found.'')') NSMAX
      IHQERR = 1
      CALL HBUG (CHQMES, 'HQSIGA', IDMQ)
 
   80 CONTINUE
 
      END
