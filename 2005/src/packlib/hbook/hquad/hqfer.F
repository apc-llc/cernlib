*
* $Id: hqfer.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqfer.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 21/12/92  08.35.00  by  John Allison
*-- Author :
      REAL FUNCTION HQFER (V)
      REAL V (*)
* Determines error on sum of multiquadrics.
* V is coordinate vector in plot coordinates.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
 
      CHARACTER*4 NAME
      INTEGER L
      INTEGER ISIG, JSIG, NWW
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION HQD
      DOUBLE PRECISION VAR, D0, D1, SS
      DOUBLE PRECISION RSIG, DSIG, SIGTMP
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL HQD
      REAL VAR, D0, D1, SS
      REAL RSIG, DSIG, SIGTMP
#endif
      DATA RSIG /1.D-3/
 
      HQFER = 0.
 
* Needs covariances from LHFIT bank.
      LCONT = LQ (LCID - 1)
      IF (LCONT .LE. 0) RETURN
      LFUNC = LQ (LCONT - 1)
      IF (LFUNC .LE. 0) RETURN
      LHFIT = LQ (LFUNC - 1)
      IF (LHFIT .LE. 0) RETURN
      LHFCO = 0
      L = LQ (LHFIT)
   10 CONTINUE
      IF (L. NE. 0)THEN
         CALL UHTOC (IQ (L - 4), 4, NAME, 4)
         IF (NAME .EQ. 'HFCO')THEN
            LHFCO = L
         ELSE
            GO TO 10
         END IF
      END IF
      IF (LHFCO .EQ. 0) RETURN
 
* Calculate gradients (take constraints, if any, into account).
      IF (IMQFUN .EQ. 1) THEN
         D0 = HQD (V)
      ELSE
         D0 = VCONST * EXP (HQD (V))
      END IF
      DO 20    ISIG = 1, NFREE
* Store fitted value temporarily.
         SIGTMP = SIGA (ISIG)
* Make a small change.
         DSIG = SIGA (ISIG) * RSIG
         SIGA (ISIG) = SIGA (ISIG) + DSIG
* Constrain parameters again if required.
         IF (NFREE .LT. NSIG) CALL HQSSV
* Calculate new value of function.
         IF (IMQFUN .EQ. 1) THEN
            D1 = HQD (V)
         ELSE
            D1 = VCONST * EXP (HQD (V))
         END IF
* Calculate gradient of function w.r.t. parameter.
         SIGGRD (ISIG) = (D1 - D0) / DSIG
* Restore original parameter.
         SIGA (ISIG) = SIGTMP
   20 CONTINUE
* Restore constraints if required.
      IF (NFREE .LT. NSIG) CALL HQSSV
 
* Calculate variance.  Assume matrix is symmetric.
      VAR = 0.
* Diagonal terms.
      DO 30    ISIG = 1, NFREE
         CALL UCOPY
     +   (Q (LHFCO + NWW * (ISIG * (ISIG + 1) / 2 - 1) + 1),
     +   SS, NWW)
         VAR = VAR + SS * SIGGRD (ISIG) ** 2
   30 CONTINUE
* Off-diagonal terms.
      DO 50    ISIG = 2, NFREE
         DO 40    JSIG = 1, ISIG - 1
            CALL UCOPY
     +      (Q (LHFCO + NWW*(ISIG*(ISIG-1) / 2 + JSIG - 1) + 1),
     +      SS, NWW)
            VAR = VAR + 2. * SS * SIGGRD (ISIG) * SIGGRD (JSIG)
   40    CONTINUE
   50 CONTINUE
 
      HQFER = SQRT (ABS (VAR))
 
      END
