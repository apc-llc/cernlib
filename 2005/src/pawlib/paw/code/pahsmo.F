*
* $Id: pahsmo.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahsmo.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.00/01 20/12/92  17.34.35  by  John Allison
*-- Author :    John Allison   08/09/92
      SUBROUTINE PAHSMO (ID, CHOPT, SENSIT, SMOOTH,
     + NPAR, CHI2, NDF, IERSMO)
      CHARACTER CHOPT*(*)
      INTEGER ID, NPAR, NDF, IERSMO
      REAL SENSIT, SMOOTH
* PAW histogram and ntuple smoothing routine.
* Services extended HISTOGRAM/SMOOTH command.
* Includes multiquadric, 353QH and spline smoothing.
*
* Input paramaters:
*   ID     = histogram ID.
*   CHOPT  = option string.  The following charcacters are recognised:
*              0 or 1: overwrite histogram contents.
*                   2: do not overwrite histogram contents.
*                   M: multiquadric smoothing.
*                   N: do not plot result of fit.
*                   Q: 353QH smoothing.
*                   S: spline smoothing.
*                   V: verbose (default for all except 1-D histogram).
*                   F: write Fortran77 function to HQUADF.DAT (multiquadric
*                        only)
*
*                 multiquadric       or           spline
*                 ------------                    ------
*   SENSIT =  sensitivity parameter  or   scale factor for no. of knots
*   SMOOTH =  smoothing parameter    or   additive constant for degree
*
* Output parameters.
*   NPAR   = no. of parameters.
*   CHI2   = chi-squared of fit.
*   NDF    = no. of degrees of freedom of fit.
*   IERSMO = 0 if all's OK.

#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcunit.inc"

      CHARACTER*80 CHOPTQ
      LOGICAL VPRINT, FFILE
      LOGICAL HEXIST
      INTEGER ISTORE, IOPT
      INTEGER NKNOTS, IKX, NX, NY
      REAL PROB

* Clear output parameters.
      NPAR   = 0
      CHI2   = 0.
      NDF    = 0
      IERSMO = 0

* Decode option string.
      CHOPTQ = CHOPT
      LCHOPQ = LENOCC (CHOPTQ)
      ISTORE = 2
      IOPT   = 1
      FFILE = .FALSE.
      IF (INDEX (CHOPT, '0') .NE. 0) ISTORE = 0
      IF (INDEX (CHOPT, '1') .NE. 0) ISTORE = 1
      IF (INDEX (CHOPT, '2') .NE. 0) ISTORE = 2
      IF (INDEX (CHOPT, 'M') .NE. 0) IOPT = 1
      IF (INDEX (CHOPT, 'Q') .NE. 0) IOPT = 2
      IF (INDEX (CHOPT, 'S') .NE. 0) IOPT = 3
      IF (INDEX (CHOPT, 'F') .NE. 0) FFILE = .TRUE.
      IF (INDEX (CHOPT, 'V') .NE. 0 .OR. I1 .EQ. 0) THEN
         VPRINT = .TRUE.
         CHOPTQ = CHOPTQ (1: LCHOPQ) // 'V'
      ELSE
         VPRINT = .FALSE.
      END IF

      IF (IOPT .EQ. 1) THEN
* Multiquadric smoothing.
         IF (FFILE) THEN
            CALL PALUNF(60,3,LUNP)
            CALL KUOPEN(LUNP,'HQUADF.DAT','UNKNOWN',ISTAT)
            CALL HSETPR('PLUN',REAL(LUNP))
         END IF
         CALL HQUAD (ID, CHOPTQ, 0, SENSIT, SMOOTH, NPAR, CHI2, NDF,
     +   FMIN, FMAX, IERSMO)
         IF (FFILE) THEN
            CALL HSETPR('PLUN',0.)
            CALL PACLOS(LUNP)
         END IF
* HQUAD's chi-squared is a genuine one, NOT chi-squared per degree of freedom.
         IF (IERSMO .NE. 0) THEN
            WRITE (LOUT, 10000)
            IF (I4 .EQ. 0) WRITE (LOUT, 10100)
         ELSE
            IF (VPRINT) THEN
               WRITE (LOUT, 10200) NPAR, CHI2, NDF
               WRITE (LOUT, 10300) FMIN, FMAX
            END IF
            IF (NDF .GT. 0) THEN
               CPROB = PROB (CHI2, NDF)
               IF (CPROB .LT. 1.E-10) WRITE (LOUT, 10400)
            END IF
         END IF
      END IF
      IF (IOPT .EQ. 2) THEN
* 353QH smoothing.
         IF (I1 .NE. 0) THEN
            CALL HSMOOF (ID, ISTORE, CHI2)
* HSMOOF's chi-squared is a genuine one, NOT chi-squared per degree of freedom,
*   but no. of parameters is a mystery.
            IF (VPRINT) WRITE (LOUT, 10500) CHI2, IQ (LCID + KNCX)
         ELSE
            WRITE (LOUT, 10600)
            IERSMO = 1
            GO TO 10
         END IF
      END IF
      IF (IOPT .EQ. 3) THEN
* Spline smoothing (cubic spline only, no. of knots variable; for more
*   freedom use SPLINE command).
         NKNOTS = 10. * SENSIT
         IKX = SMOOTH + 2
         IF (I1 .NE. 0) THEN
            NX = IQ (LCID + KNCX)
            NPAR = NKNOTS - IKX - 1
            NDF = NX - NPAR
            CALL HSPLI1 (ID, ISTORE, NKNOTS, IKX, CHI2)
* HSPLI1's chi-squared is NOT a genuine one, it is chi-squared per degree of
*   freedom.
            IF (VPRINT) THEN
               WRITE (LOUT, 10700) NKNOTS, IKX, NPAR, CHI2 * NDF, NDF
               WRITE (LOUT, 10800)
            END IF
         ELSE IF (I230 .NE. 0) THEN
            IF (ISTORE .EQ. 0 .OR. ISTORE .EQ. 1) THEN
               NX = IQ (LCID + KNCX)
               NY = IQ (LCID + KNCY)
               NPAR = (NKNOTS - IKX - 1) ** 2
               NDF = NX * NY - NPAR
               CHI2 = 0.
               CALL HSPLI2 (ID, NKNOTS, NKNOTS, IKX, IKX)
               IF (VPRINT) WRITE (LOUT, 10900) NKNOTS, IKX, NPAR
            ELSE
               WRITE (LOUT, 11000)
               IERSMO = 1
               GO TO 10
            ENDIF
            WRITE (LOUT, 10800)
         ELSE
            WRITE (LOUT, 11100)
            IERSMO = 1
            GO TO 10
         ENDIF
      END IF

* Redraw histogram with fitted function.
      IF (HEXIST (ID).AND.INDEX(CHOPT,'N').EQ.0)THEN
         CALL HPLOT (ID, ' ', ' ', 0)
      ENDIF

   10 CONTINUE

10000 FORMAT (1X, 'Unable to do multiquadric smoothing.')
10100 FORMAT (3X, 'Try another method - OPTION = Q or S.')
10200 FORMAT (1X, 'Multiquadric smoothing with', I4,  ' parameters.'/
     +3X,'  Chi-squared', G12.5, ' for', I7, ' degrees of freedom.')
10300 FORMAT (3X, 'Min/max event density:', 2G12.5)
10400 FORMAT (3X, 'Chi-squared probability is very low.'/
     +3X, 'Try a larger sensitivity parameter (e.g., SMOOTH id ! 1.5)'/
     +3X, 'Are the data genuinely random?  Are the bin contents ',
     +'independent?'/
     +3X, 'Are the errors correct?  (This method assumes data are'/
     +3X, 'randomly drawn from their parent probability distribution.)')
10500 FORMAT (1X, '353QH smoothing.' / '  Chi-squared',
     +G12.5, ' for', I7, ' bins.')
10600 FORMAT (1X, '353QH smoothing not available for 2-D or ntuples.')
10700 FORMAT (1X, 'Spline smoothing with', I3, ' knots, degree', I2,
     +' and', I4, ' parameters.'/
     +3X, 'Chi-squared', G12.5, ' for', I7, ' degrees of freedom.')
10800 FORMAT (3X,
     +'(Note: the SPLINE command gives you more flexibility.)')
10900 FORMAT (1X, 'Spline smoothing with', I3, ' knots, degree', I2,
     +' and', I4, ' parameters.')
11000 FORMAT (3X, '2-D spline smoothing routine HSPLI2 always ',
     +'overwrites.'/
     +3X, 'Specify option 1S to confirm.')
11100 FORMAT (1X, 'Spline smoothing not available for ntuples.')
*
      END
