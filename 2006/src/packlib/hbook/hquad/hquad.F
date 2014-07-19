*
* $Id: hquad.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hquad.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/08 04/07/94  08.55.16  by  Rene Brun
*-- Author :
      SUBROUTINE HQUAD (IDHQ, CHOPTQ, IMODE, SENSI0, SMOOT0, NSIG1,
     +CHISQ, NDF, FMIN1, FMAX1, IHQERR)
      INTEGER IDHQ, IMODE, NSIG1, NDF, IHQERR
      CHARACTER*(*) CHOPTQ
      REAL SENSI0, SMOOT0, CHISQ, FMIN1, FMAX1
* Peform no-nonsense fit with multiquadric radial basis functions.
* 1- and 2-D histograms and ntuples with up to NDMAX variables.
*
* Writes Fortran77 function to unit PLUN if set by HSETPR.
*
* Input variables:
*   IDHQ   = histogram or ntuple ID.
*   CHOPTQ = contains option characters:
*              0 or 1: replace original histogram by smoothed.
*                   2: store values of smoothed function and its parameters
*                        without replacing original histogram - results are
*                        available as superimposed curve (1-D) or otherwise
*                        when editing (displaying) - see HPLOT options.
*                   V: verbose.
*                   F: write FORTRAN function on unit given by HSETPR.
*   IMODE is synonymous with MODE.
*   MODE   < 0, use existing significant points.
*****************************************************************************
************* Note: MODE < 0 needs re-thinking.  Existing points can now only
*************        be picked up from an already fitted histogram or ntuple.
*****************************************************************************
*   MODE   = 0, equivalent to MODE = 3.
*   MODE0  = ABS (MODE) = 10 * MODE2 + MODE1, where
*   MODE1  = 1, simply fill MQ histogram banks (L1H, L1V, etc.).
*          = 2, find significant points and interpolate.
*          = 3, find significant points and perform unconstrained fit.  If the
*                 histogram or ntuple is unweighted (mean variance = 1.)
*                 perform a Poisson likelihood fit, otherwise a least squares
*                 fit.
*          = 4, find significant points and perform unconstrained least squares
*                 fit.  (This is a linear least squares problem and therefore
*                 the most efficient possible since it allows a single step
*                 calculation of the best fit and covariances.  But note it
*                 assumes gaussian errors, including the error on zero is equal
*                 to 1.)
*          = 5, find significant points and perform least squares constrained
*                 fit, constrained for zero asyptotes, if possible, otherwise
*                 default to unconstrained fit.  (Not currently implemented.)
*          = 6, find significant points and perform unconstrained Poisson
*                 likelihood fit.
*   MODE2  = 0, do not perform maximum likelihood fit to events.
*          > 0, perform maximum likelihood fit to events (ntuple only).
*
*   SENSI0 = SENSIT, a sensitivity parameter (if = 0., SENSIT = default (1.)).
*   SMOOT0 = SPREAD, a smoothness parameter (if = 0., SPREAD = default (1.)).
*
* Output variables:
*   NSIG1  = no. of significant points found.
*   CHISQ  = chi-squared of result.
*   NDF    = no. of degrees of freedom.
*   FMIN1, FMAX1 = minimum and maximum function values, i.e., event densities.
*   IHQERR = 0 if all's OK.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcpar0.inc"
 
* Miscellaneous variables.
      CHARACTER*80 CHQMES
      CHARACTER*8 TAGS (NDMAX)
      LOGICAL HEXIST
      CHARACTER*80 TITLE
      INTEGER MODE, MODE0, MODE1, MODE2, IDN, ISEL
      INTEGER NVAR, NSMINN, NNBINN, NFACTR, NTRIES
      INTEGER IEDGE, NWT, LOC, LCOV, LDER, LWK1, LWK2, LWK3, NWW
      INTEGER I, IDIM, IX, IY, IZ
      REAL FACTOR
      REAL V (NDMAX)
      REAL X, Y, Z
      EQUIVALENCE (X, V (1)), (Y, V (2)), (Z, V (3))
      REAL F, HQF, ALOGLI, CHI2
      EXTERNAL HQF
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
#endif
 
* Store requested ID in /HCQCOM/.
      IDMQ = IDHQ
 
* Check option characters.
      IF (INDEX (CHOPTQ, '0') .NE. 0 .OR. INDEX (CHOPTQ, '1') .NE. 0)
     +THEN
         ISEL = 1
      ELSE
         ISEL = 2
      END IF
      IF (INDEX (CHOPTQ, 'V') .NE. 0 .OR. INDEX (CHOPTQ, 'v') .NE. 0)
     +THEN
         VPRINT = .TRUE.
      ELSE
         VPRINT = .FALSE.
      END IF
 
* Check MODE.
      MODE = IMODE
      IF (MODE .LT. 0) GO TO 100
* MODE = 0 is equivalent to MODE = 3.
      IF (MODE. EQ. 0) MODE = 3
      MODE0 = ABS (MODE)
      MODE1 = MOD (MODE0, 10)
      MODE2 = MOD (MODE0 / 10, 10)
      IF (MODE1. GT. 6) GO TO 110
* Constrained fit not currently implemented.
      IF (MODE1 .EQ. 5) GO TO 120
 
* Clear IHQERR and other variables.
      NSIG1  = 0
      CHISQ  = 0.
      NDF    = 0
      IHQERR = 0
 
* Set some /HCQCOM/ parameters.
      IF (SENSI0 .EQ. 0.) THEN
         SENSIT = 1.
      ELSE
         SENSIT = SENSI0
      END IF
      IF (SMOOT0 .EQ. 0.) THEN
         SPREAD = 1.
      ELSE
         SPREAD = SMOOT0
      END IF
      SMALLD = 1.E-6
      VSCALE = 1.
 
* Check existence of histogram or ntuple and fill related parameters
*   in /HCQCOM/.
      NTUPLE = .FALSE.
      IDN = IDHQ
      NVAR = NDMAX
      CALL HGIVEN (IDN, TITLE, NVAR, TAGS, SIGVMI, SIGVMA)
      IF (NVAR .GT. 0) THEN
         NTUPLE = .TRUE.
         IF (NVAR .GT. NSMAX) GO TO 140
         CALL HNOENT (IDN, NMQEVS)
         NDIM = NVAR
         IF (NDIM .EQ. 1) THEN
            NSMINN = NSMIN1
            NNBINN = NNBIN1
         ELSE IF (NDIM .EQ. 2) THEN
            NSMINN = NSMIN2
            NNBINN = NNBIN2
         ELSE IF (NDIM .EQ. 3) THEN
            NDIM = 3
            NSMINN = NSMIN3
            NNBINN = NNBIN3
         ELSE
            WRITE (LOUT, '(3X, ''Taking 1st 3 variables.'')')
            NDIM = 3
            NSMINN = NSMIN3
            NNBINN = NNBIN3
         END IF
         NSMIN  = NSMINN
         NBTOT  = 1
         VOLTOT = 1.
         VOLBIN = 1.
         DO 10    IDIM = 1, NDIM
            NBTOT = NBTOT * NNBINN
            NBINS  (IDIM) = NNBINN
            NBINSN (IDIM) = NNBINN
            SIGVT  (IDIM) = SIGVMA (IDIM) - SIGVMI (IDIM)
            SIGVBI (IDIM) = SIGVT (IDIM) / NBINS (IDIM)
            VOLTOT = VOLTOT * SIGVT  (IDIM)
            VOLBIN = VOLBIN * SIGVBI (IDIM)
   10    CONTINUE
      ELSE IF (HEXIST (IDHQ)) THEN
         CALL HGIVE (IDHQ, TITLE, NX, XMI, XMA, NY, YMI, YMA, NWT, LOC)
         IF (NY .EQ. 0) THEN
            NDIM = 1
            NSMIN = NSMIN1
            NBTOT = NX
            DXT = XMA - XMI
            DX = DXT / NX
            VOLTOT = DXT
            VOLBIN = DX
         ELSE
            NDIM = 2
            NSMIN = NSMIN2
            NBTOT = NX * NY
            DXT = XMA - XMI
            DX = DXT / NX
            DYT = YMA - YMI
            DY = DYT / NY
            VOLTOT = DXT * DYT
            VOLBIN = DX * DY
         END IF
      ELSE
         GO TO 130
      END IF
      IDMQ = IDHQ
 
* Allocate working space needed....
      NSIG = 0
      CALL HQINIT (NSIG, NDIM, NBINS, IHQERR)
      IF (IHQERR. NE. 0) GO TO 170
 
* Lift LQ (LHQUAD - 1) chain, L1H etc.
      CALL HQLIF1 (IHQERR)
      IF (IHQERR. NE. 0) GO TO 170
 
* Get contents and errors.
      NTRIES = 0
   20 CONTINUE
      CALL HQBIN (NBINS, 0, IHQERR)
      IF (IHQERR. NE. 0) GO TO 170
      NTRIES = NTRIES + 1
 
* MODE1 = 1 work finished.
      IF (MODE1 .EQ. 1) GO TO 170
 
* Find significant points (HQSIG overwrites data).
      CALL HQSIG (IEDGE, IHQERR)
      IF (IHQERR .NE. 0) THEN
         IF (NTRIES .GT. 10) GO TO 170
* If too few points found, check mean significance.
         IF (IHQERR .EQ. 20) THEN
* Suspect errors incorrectly assigned (HBARX not called) - scale.
            IF (NTRIES .EQ. 1) THEN
* First guess - experience shows...
               VSCALE = VSCALE * SLRMS ** 2 / 4.
               NSMIN = 2 * NSMIN
            ELSE
               VSCALE = VSCALE / 4.
            END IF
            WRITE (LOUT, 10000) SQRT (VSCALE)
            IHQERR = 0
            NSIG = 0
            GO TO 20
* If too many points, reduce bins and sensitivity (ntuples only).
         ELSE IF (NTUPLE .AND. (IHQERR .EQ. 1 .OR. IHQERR .EQ. 30))
     +   THEN
            FACTOR = 1.1
            NFACTR = 10. * FACTOR
            NBTOT = 1
            VOLBIN = 1.
            DO 30 IDIM = 1, NDIM
               IF (NBINS (IDIM) .LT. NFACTR) GO TO 170
               NBINS (IDIM) = NBINS (IDIM) / FACTOR
               NBINSN (IDIM) = NBINS (IDIM)
               NBTOT = NBTOT * NBINS (IDIM)
               SIGVBI (IDIM) = SIGVT (IDIM) / NBINS (IDIM)
               VOLBIN = VOLBIN * SIGVBI (IDIM)
   30       CONTINUE
            SENSIT = SENSIT / FACTOR
            IF (VPRINT) THEN
               WRITE (LOUT, 10100) SENSIT
               IF (NDIM .EQ. 1) THEN
                  WRITE (LOUT, 10200) NX
               ELSE IF (NDIM .EQ. 2) THEN
                  WRITE (LOUT, 10300) NX, NY
               ELSE IF (NDIM .EQ. 3) THEN
                  WRITE (LOUT, 10400) NX, NY, NZ
               ELSE
                  GO TO 150
               END IF
            END IF
            IHQERR = 0
            NSIG = 0
            GO TO 20
         END IF
      END IF
 
* Adjust to "best" histogram (no. of bins NXN, etc., possibly changed by HQSIG1,
*   etc.) to get starting values for maximum likelihood fit to events.
*   (Note: you get into trouble if you try fitting a coarser histogram to sig.
*   points which have been determined with a finer one.)
* (The above is nonsense.  The Poisson likelihood works very well for sparse
*   histograms!  Comment out!)
*     IF (NTUPLE) THEN
*        NBTOT  = 1
*        VOLBIN = 1.
*        DO xx IDIM = 1, NDIM
*           NBINS (IDIM) = NBINSN (IDIM)
*           NBTOT = NBTOT * NBINS (IDIM)
*           SIGVBI (IDIM) = SIGVT (IDIM) / NBINS (IDIM)
*           VOLBIN = VOLBIN * SIGVBI (IDIM)
*  xx CONTINUE
*     END IF
 
* Lift LHQCOV bank, etc...
      CALL HQLIF2 (IHQERR)
      IF (IHQERR. NE. 0) GO TO 170
* ...and optional banks LHQDJN, etc.
      CALL HQLIF3
 
* Get contents and errors again, and compute variances.
* Set variance of empty bins equal to square of mean variance for least squares
*   fitting in "extreme gaussian approximation" (2nd argument of HQBIN = 1).
*   (For unweighted histograms, Vmean = 1.  For histograms which are simply
*   scaled unweighted histograms, e.g., normalised histograms, the scale
*   factor is 1./Vmean.  In both cases, the "variance of empty bins in the
*   extreme gaussian approximation" is Vmean**2.  For other weighted histograms
*   with empty bins, this may not be a good assumption.)
      CALL HQBIN (NBINS, 1, IHQERR)
      IF (IHQERR. NE. 0) GO TO 170
 
* Perform fit/interpolation.  All parameters are free unless
*   otherwise changed by these routines.
      NFREE = NSIG
 
* IF = DOUBLE, make sure pointers are even so arrays start on 8-byte boundary.
      LCOV = LHQCOV + MOD (LHQCOV, NWW)
      LDER = LHQDER + MOD (LHQDER, NWW)
      LWK1 = LHQWK1 + MOD (LHQWK1, NWW)
      LWK2 = LHQWK2 + MOD (LHQWK2, NWW)
      LWK3 = LHQWK3 + MOD (LHQWK3, NWW)
 
      IF (MODE1 .EQ. 2) THEN
 
* Interpolate from bin heights at the significant points.
         CALL HQSOLV (Q (LCOV + 1), Q (LWK1 + 1), NSIG, CHISQ, IHQERR)
 
      ELSE IF (MODE1 .EQ. 3) THEN
 
         IF (ABS (VMEAN1 - 1.) .LT. 1.E-3) THEN
* Unconstrained Poisson likelihood fit.
            CALL HQPOIS (Q (LCOV + 1), Q (LDER + 1), Q (LWK1 + 1),
     +      Q (LWK2 + 1), Q (LWK3 + 1), NSIG, CHISQ, ALOGLI, IHQERR)
         ELSE
* Unconstrained, linear least squares fit.
            CALL HQLSQU (Q (LCOV + 1), Q (LWK1 + 1), NSIG, CHISQ,
     +      IHQERR)
         END IF
 
      ELSE IF (MODE1 .EQ. 4) THEN
 
* Unconstrained, linear least squares fit.
         CALL HQLSQU (Q (LCOV + 1), Q (LWK1 + 1), NSIG, CHISQ, IHQERR)
 
      ELSE IF (MODE1 .EQ. 5) THEN
 
* Constrained fitting if edges are zero (IEDGE .NE. 0), otherwise default to
*   unconstrained fit.
***** No contsrained fitting yet.
*        IF (IEDGE .EQ. 0) THEN
         CALL HQLSQU (Q (LCOV + 1), Q (LWK1 + 1), NSIG, CHISQ, IHQERR)
*        ELSE
* Perform least squares fit constrained for zero asymptotes.
*           CALL HQLSQC (Q (LCOV + 1), Q (LWK1 + 1), NSIG, CHISQ,
*    +      IHQERR)
*        END IF
 
      ELSE IF (MODE1 .EQ. 6) THEN
 
* Unconstrained Poisson likelihood fit.
         CALL HQPOIS (Q (LCOV + 1), Q (LDER + 1), Q (LWK1 + 1),
     +   Q (LWK2 + 1), Q (LWK3 + 1), NSIG, CHISQ, ALOGLI, IHQERR)
 
      END IF
 
* Check for error.
      IF (IHQERR .NE. 0) GO TO 170
 
* Perform unbinned maximum likelihood fit to events.
* (Not currently implemented.)
      IF (MODE2 .GT. 0) THEN
         WRITE (LOUT, 10500)
         MODE2 = 0
      END IF
 
      IF (NTUPLE .AND. MODE2 .GT. 0) THEN
 
         IF (IMQFUN .LE. 1) THEN
 
            WRITE (LOUT, 10600)
 
         ELSE
 
* Maximise the event likelihood.
*           CALL HQMXLI (Q (LCOV + 1), Q (LWK1 + 1), Q (LWK2 + 1), NSIG,
*    +      CHISQ, ALOGLI, IHQERR)
            IF (IHQERR .NE. 0) GO TO 160
 
         END IF
 
      END IF
 
* Replace contents (use Q (LnH.. as workspace) or store as function.
      HQFMIN =  1.E20
      HQFMAX = -1.E20
      IF (NDIM .EQ. 1) THEN
         DO 40    IX = 1, NX
            X = XMI + (IX - 0.5) * DX
            I = IX
            F = HQF (V)
            IF (HQFMIN .GT. F) HQFMIN = F
            IF (HQFMAX .LT. F) HQFMAX = F
            Q (L1H + I) = F
   40    CONTINUE
         IF (.NOT. NTUPLE .AND. ISEL .LE. 1) THEN
            CALL HPAK (IDMQ, Q (L1H + 1))
         END IF
      ELSE IF (NDIM .EQ. 2) THEN
         DO 60    IX = 1, NX
            X = XMI + (IX - 0.5) * DX
            DO 50    IY = 1, NY
               Y = YMI + (IY - 0.5) * DY
               I = (IY - 1) * NX + IX
               F = HQF (V)
               IF (HQFMIN .GT. F) HQFMIN = F
               IF (HQFMAX .LT. F) HQFMAX = F
               Q (L2H + I) = F
   50       CONTINUE
   60    CONTINUE
         IF (.NOT. NTUPLE .AND. ISEL .LE. 1) THEN
            CALL HPAK (IDMQ, Q (L2H + 1))
         END IF
      ELSE IF (NDIM .EQ. 3) THEN
         DO 90    IX = 1, NX
            X = XMI + (IX - 0.5) * DX
            DO 80    IY = 1, NY
               Y = YMI + (IY - 0.5) * DY
               DO 70    IZ = 1, NZ
                  Z = ZMI + (IZ - 0.5) * DZ
                  F = HQF (V)
                  IF (HQFMIN .GT. F) HQFMIN = F
                  IF (HQFMAX .LT. F) HQFMAX = F
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
      ELSE
         GO TO 150
      END IF
      IF (.NOT. NTUPLE .AND. ISEL .LE. 1) THEN
         HQMIN = HQFMIN
         HQMAX = HQFMAX
      END IF
 
* Fill output variables.
      IF (VSCALE .NE. 1.) CHISQ = 0.
      NSIG1 = NSIG
      NDF   = NBTOT - NSIG
      FMIN1 = HQFMIN
      FMAX1 = HQFMAX
 
* Fill LHFIT banks.
* To conform to PAW conventions, quote chi-squared per degree of freedom.
      IF (NDF .GT. 0) CHI2 = CHISQ / NDF
* Store fit parameters.
      NFPAR=NSIG1
      NPFITS=NFPAR+NDF
      FITCHI=CHI2
      FITNAM(1)='MQuadric'
      IF (NTUPLE) THEN
         CALL HSUPIN(0)
         CALL HSUPIN(1)
         CALL HSUPIN(3)
      ELSE IF (ISEL .GT. 1) THEN
         CALL HSUPIS(HQF,0,0,0)
         CALL HSUPIS(HQF,1,0,0)
         CALL HSUPIS(HQF,3,0,0)
      END IF
* Note: the above call to HSUPIS with 2nd arg = 3 causes it to store values of
*   the fit at bin centres in the LFUNC bank for both 1- and 2-D histograms.  It
*   also stores the fit parameters and their errors in the LHFIT bank.  A
*   typical 40x40 2-D histogram needing, say, 148 multiquadric basis functions
*   to describe it in an low-bias way requires:
*                                                             words
*      LFUNC bank                                              1600
*      LHFIT banks (double precision):
*       148 coefficients of basis function             148
*       other parameters - 148 * (NDIM + 1)            444
*       errors on coefficients                         148
*       lower triangle of covariance matrix          11026
*                                                    -----
*                                                    11766 =  25352
*                                                    -----
 
* Write Fortran77 function to unit PLUN if set by HSETPR.
      IF (PLUN .GT. 0. .AND. PLUN .LT. 100.) THEN
         CALL HQWRIF (INT (PLUN))
      END IF
 
      GO TO 170
 
* Errors
  100 CONTINUE
      CHQMES = 'MODE < 0 not implemented.'
      IHQERR = 100
      GO TO 160
  110 CONTINUE
      WRITE (CHQMES, '(''Mode'', I2, '' not implemented.'')') MODE
      IHQERR = 200
      GO TO 160
  120 CONTINUE
      WRITE (CHQMES,
     + '(''Mode'', I2, '' not currently implemented.'')') MODE
      IHQERR = 400
      GO TO 160
  130 CONTINUE
      WRITE (CHQMES, '(''No histogram or ntuple with id ='', I7)')
     +IDHQ
      IHQERR = 500
      GO TO 160
  140 CONTINUE
      WRITE (CHQMES, '(''Ntuple has more than'', I3, '' variables.'')')
     +NDMAX
      IHQERR = 600
      GO TO 160
  150 CONTINUE
      WRITE (CHQMES, '(''Not programmed for'', I3,
     +'' dimensions.'')') NDIM
      IHQERR = 700
      GO TO 160
 
  160 CONTINUE
      CALL HBUG (CHQMES, 'HQUAD', IDMQ)
 
  170 CONTINUE
 
* Drop banks, etc.
      CALL HQEND
 
10000 FORMAT (1X, 'Warning: statistically insignificant.'/
     +3X, 'Errors being scaled by', G10.3)
10100 FORMAT (1X, 'Trying to reduce no. of significant points:'/
     +3X, 'A) by reducing sensitivity parameter to', F10.4)
10200 FORMAT (3X, 'B) by reducing no. of bins to', I5)
10300 FORMAT (3X, 'B) by reducing no. of bins to', I3, ' x', I3)
10400 FORMAT (3X, 'B) by reducing no. of bins to', I3, ' x', I3,
     +' x', I3)
10500 FORMAT (1X, 'Unbinned maximum likelihood still under development',
     +' - try later!')
10600 FORMAT (
     +1X, 'HQUADN: weighted event likelihood not programmed yet.'/
     +3X, 'Fit is result of least squares fit to the MQ histogram.')
 
      END
