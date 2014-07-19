*=========================================================================
*
* $Id: hqmnu.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hqmnu.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  15.16.56  by  John Allison
*-- Author :
 
      SUBROUTINE HQMNU (NPAR, GRAD, DCHISQ, SIGAA, IFLAG)
      INTEGER NPAR, IFLAG
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION GRAD (*), DCHISQ, SIGAA (*)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL GRAD (*), DCHISQ, SIGAA (*)
#endif
* For MINUIT minimisation of chi-squared.
* Fit to histogram contents.
* Work in NORMALISED coordinates.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
 
      CHARACTER*80 CHQMES
      INTEGER J, L, IX, IY, IZ
      REAL V (NDMAX)
      REAL X, Y, Z
      EQUIVALENCE (X, V (1)), (Y, V(2)), (Z, V(3))
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DCHI, HQDN, DN
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL DCHI, HQDN, DN
#endif
 
      DO 10    J = 1, NPAR
         SIGA (J) = SIGAA (J)
   10 CONTINUE
 
* For constrained fit...
      IF (NPAR .LT. NSIG) CALL HQSSV
 
      DCHISQ = 0.
 
*      WRITE (LOUT, 10000)
 
      IF (NDIM .EQ. 1) THEN
         DO 20    IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            IF (IMQFUN .EQ. 1) THEN
               DN = HQDN (V)
            ELSE
               DN = VCONST * EXP (HQDN (V))
            END IF
            DCHISQ = DCHISQ +
     +      (Q (L1H + L) - DN)**2 / Q (L1V + L)
   20    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 40    IX = 1, NX
            X = (IX - 0.5) / NX
            DO 30    IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               IF (IMQFUN .EQ. 1) THEN
                  DN = HQDN (V)
               ELSE
                  DN = VCONST * EXP (HQDN (V))
               END IF
               DCHISQ = DCHISQ +
     +         (Q (L2H + L) - DN)**2 / Q (L2V + L)
   30       CONTINUE
   40    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 70    IX = 1, NX
            X = (IX - 0.5) / NX
            DO 60    IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 50    IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  IF (IMQFUN .EQ. 1) THEN
                     DN = HQDN (V)
                  ELSE
                     DN = VCONST * EXP (HQDN (V))
                  END IF
                  DCHI = (Q (L3H + L) - DN) ** 2 / Q (L3V + L)
*      WRITE (LOUT, 10100) X, Y, Z, Q (L3H + L), Q (L3V + L),
*     + DN, DCHI
                  DCHISQ = DCHISQ + DCHI
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
      ELSE
         GO TO 80
      END IF
 
 
      GO TO 90
 
   80 CONTINUE
      WRITE (CHQMES, '(''HQMNU:'', I3, '' dimensions not programmed '
     +//'yet.'')') NDIM
      CALL HBUG (CHQMES, 'HQMNU', IDMQ)
 
   90 CONTINUE
 
10000 FORMAT (1X, 'HQMNU: X, Y, Z, H, V, FIT, DCHI:')
10100 FORMAT (1X, 7G11.4)
 
      END
