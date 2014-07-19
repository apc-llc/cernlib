*
* $Id: igrtoh.F,v 1.1.1.1 1996/02/14 13:10:38 mclareni Exp $
*
* $Log: igrtoh.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/07 01/11/91  11.46.59  by  O.Couet
*-- Author :
      SUBROUTINE IGRTOH(RED,GREEN,BLUE,RHUE,RLIGHT,RSATUR)
*.===========>
*.
*.   Convert RGB to HSL
*.
*..==========> (L.Roberts)
      REAL RNORM, GNORM, BNORM, MINVAL, MAXVAL, MSUM, MDIFF
*.______________________________________
*
      MINVAL = MIN (RED, GREEN, BLUE)
      MAXVAL = MAX (RED, GREEN, BLUE)
*
      RNORM = 0.0
      GNORM = 0.0
      BNORM = 0.0
      MDIFF = MAXVAL - MINVAL
      MSUM = MAXVAL + MINVAL
      IF(MAXVAL .NE. MINVAL)THEN
         RNORM = (MAXVAL - RED) / MDIFF
         GNORM = (MAXVAL - GREEN) / MDIFF
         BNORM = (MAXVAL - BLUE) / MDIFF
      ENDIF
*
      RLIGHT = MSUM / 2.0
*
      IF (MAXVAL.EQ.MINVAL)THEN
         RSATUR = 0.0
         RHUE = 0.0
         GOTO 30
      ENDIF
*
      IF (RLIGHT .LE. 0.5) RSATUR = MDIFF / MSUM
      IF (RLIGHT .GT. 0.5) RSATUR = MDIFF / (2.0 - MSUM)
*
      IF (RED .EQ. MAXVAL) RHUE = 60.0 * (6.0 + BNORM - GNORM)
      IF (GREEN .EQ. MAXVAL) RHUE = 60.0 * (2.0 + RNORM - BNORM)
      IF (BLUE .EQ. MAXVAL) RHUE = 60.0 * (4.0 + GNORM - RNORM)
*
   30 IF (RHUE .GT. 360.0) RHUE = RHUE - 360.0
*
      END
