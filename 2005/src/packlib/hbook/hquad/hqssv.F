*
* $Id: hqssv.F,v 1.1.1.1 1996/01/16 17:08:06 mclareni Exp $
*
* $Log: hqssv.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:06  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.13.37  by  John Allison
*-- Author :
      SUBROUTINE HQSSV
* Calculate the NDIM + 1 dependent variables, the ones that
*   ensure sum and sum (strength * position) are zero.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
 
      CHARACTER*80 CHQMES
      INTEGER ISIG
      INTEGER J1, J2, J3
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION A, B, C, D
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL A, B, C, D
#endif
 
      IF (NDIM .EQ. 1) THEN
 
         A = 0.
         B = 0.
         DO 10    ISIG = 1, NSIG - 2
            A = A + SIGA (ISIG)
            B = B + SIGA (ISIG) * SIGX (ISIG)
   10    CONTINUE
         J1 = NSIG - 1
         J2 = J1 + 1
         D = SIGX (J2) - SIGX (J1)
         SIGA (J1) =  (B - A * SIGX (J2)) / D
         SIGA (J2) = -(B - A * SIGX (J1)) / D
 
      ELSE IF (NDIM .EQ. 2) THEN
 
         A = 0.
         B = 0.
         C = 0.
         DO 20    ISIG = 1, NSIG - 3
            A = A + SIGA (ISIG)
            B = B + SIGA (ISIG) * SIGX (ISIG)
            C = C + SIGA (ISIG) * SIGY (ISIG)
   20    CONTINUE
         J1 = NSIG - 2
         J2 = J1 + 1
         J3 = J2 + 1
         D = SIGX (J2) * SIGY (J3) - SIGX (J3) * SIGY (J2) +
     +   SIGX (J3) * SIGY (J1) - SIGX (J1) * SIGY (J3) +
     +   SIGX (J1) * SIGY (J2) - SIGX (J2) * SIGY (J1)
         SIGA  (J1) =  -  (SIGX (J3) * C - B * SIGY (J3) +
     +   B * SIGY (J2) - SIGX (J2) * C +
     +   A * (SIGX (J2) * SIGY (J3) - SIGX (J3) * SIGY (J2))) / D
         SIGA  (J2) =  (B * SIGY (J1) - SIGX (J1) * C +
     +   A * (SIGX (J1) * SIGY (J3) - SIGX (J3) * SIGY (J1)) +
     +   SIGX (J3) * C - B * SIGY (J3)) / D
         SIGA  (J3) =  -  (A * (SIGX (J1) * SIGY (J2) -
     +   SIGX (J2) * SIGY (J1)) +
     +   SIGX (J2) * C - B * SIGY (J2) +
     +   B * SIGY (J1) - SIGX (J1) * C) / D
 
      ELSE
 
         WRITE (CHQMES, '(''Not programmed for'', I3,'
     +   //'        '' dimensions.'')') NDIM
         CALL HBUG (CHQMES, 'HQSSV', IDMQ)
 
      END IF
 
      END
