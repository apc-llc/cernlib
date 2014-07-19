*
* $Id: rgtohs.F,v 1.1.1.1 1996/02/14 13:10:30 mclareni Exp $
*
* $Log: rgtohs.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/03 21/12/88  18.27.22  by  O.Couet
*-- Author :
      SUBROUTINE RGTOHS (RED, GREEN, BLUE, HUE, SATUR, LIGHT)
*.===========>
*.
*.   Convert RGB to HSL
*.
*..==========> (L.Roberts)
C
C
      REAL               RED, GREEN, BLUE
      INTEGER                              HUE, SATUR, LIGHT
C
      REAL RNORM, GNORM, BNORM, MINVAL, MAXVAL, MSUM, MDIFF
      REAL RLIGHT, RHUE, RSATUR
C
C     ***** CONVERT RGB TO HSL
C
C
C           ***** Find the maximum and minimum of the RGB values.
C
      MINVAL = MIN (RED, GREEN, BLUE)
      MAXVAL = MAX (RED, GREEN, BLUE)
C
C           ***** Find the normalized RGB values.
C
      RNORM = 0.0
      GNORM = 0.0
      BNORM = 0.0
      MDIFF = MAXVAL - MINVAL
      MSUM = MAXVAL + MINVAL
      IF (MAXVAL .EQ. MINVAL) GO TO 1000
      RNORM = (MAXVAL - RED) / MDIFF
      GNORM = (MAXVAL - GREEN) / MDIFF
      BNORM = (MAXVAL - BLUE) / MDIFF
C
 1000 CONTINUE
C
C           ***** Calculate the lightness.
C
      RLIGHT = MSUM / 2.0
C
C           ***** If the maximum and minimum RGB values are equal,
C           ***** then the saturation and hue are both 0.
C
      IF (MAXVAL .NE. MINVAL) GO TO 2000
      RSATUR = 0.0
      RHUE = 0.0
      GO TO 3000
C
 2000 CONTINUE
C
C              ***** Calculate the saturation.
C
      IF (RLIGHT .LE. 0.5) RSATUR = MDIFF / MSUM
      IF (RLIGHT .GT. 0.5) RSATUR = MDIFF / (2.0 - MSUM)
C
C              ***** Calculate the hue.
C
      IF (RED .EQ. MAXVAL) RHUE = 60.0 * (2.0 + BNORM - GNORM)
      IF (GREEN .EQ. MAXVAL) RHUE = 60.0 * (4.0 + RNORM - BNORM)
      IF (BLUE .EQ. MAXVAL) RHUE = 60.0 * (6.0 + GNORM - RNORM)
C
 3000 CONTINUE
      IF (RHUE .GT. 360.0) RHUE = RHUE - 360.0
C
      HUE = IFIX(RHUE)
      LIGHT = IFIX(RLIGHT * 32767.0)
      SATUR = IFIX(RSATUR * 32767.0)
C
99999 CONTINUE
C
      RETURN
      END
#endif
