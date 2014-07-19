*
* $Id: hqgetf.F,v 1.1.1.1 1996/01/16 17:08:02 mclareni Exp $
*
* $Log: hqgetf.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:02  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.13.37  by  John Allison
*-- Author :    John Allison   12/09/92
      SUBROUTINE HQGETF (LFIT)
      INTEGER LFIT
* Get Multiquadric parameters from bank 'HFIT' and place in /HCQCOM/.
* See HSUPIS for banks definitions.
 
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
 
      INTEGER I, II, NBLOCK, LLIMS
      INTEGER NWW
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL SS
#endif
 
      NSIG = 0
 
      IF (LFIT .LE. 0) RETURN
 
      NSIG   = IQ (LFIT + 2)
 
      IF (I1 .NE. 0) THEN
         NDIM = 1
*        NX  = IQ (LCID + KNCX)
         XMI =  Q (LCID + KXMIN)
         XMA =  Q (LCID + KXMAX)
         DXT = XMA - XMI
      ELSE IF (I230 .NE. 0) THEN
         NDIM = 2
*        NX  = IQ (LCID + KNCX)
         XMI =  Q (LCID + KXMIN)
         XMA =  Q (LCID + KXMAX)
         DXT = XMA - XMI
         NY  = IQ (LCID + KNCY)
         YMI =  Q (LCID + KYMIN)
         YMA =  Q (LCID + KYMAX)
         DYT = YMA - YMI
      ELSE IF (I4 .NE. 0) THEN
         NDIM = IQ (LCID + 2)
         IF (NDIM .GT. 3) GO TO 60
         LLIMS = LQ (LCID - 2)
         XMI = Q (LLIMS + 1)
         XMA = Q (LLIMS + 2)
         DXT = XMA - XMI
         IF (NDIM .GE. 2) THEN
            YMI = Q (LLIMS + 3)
            YMA = Q (LLIMS + 4)
            DYT = YMA - YMI
         END IF
         IF (NDIM .GE. 3) THEN
            ZMI = Q (LLIMS + 5)
            ZMA = Q (LLIMS + 6)
            DZT = ZMA - ZMI
         END IF
      ELSE
         GO TO 50
      END IF
 
      HQFMIN = Q (LFIT + 7)
      HQFMAX = Q (LFIT + 8)
 
* LFIT bank: NSIG fitted parameters,
*            5 special parameters,
*            (NDIM + 1) * NSIG fixed parameters,
      NBLOCK = NWW * NSIG
      II = 11
      CALL UCOPY (Q (LFIT + II), SIGA, NBLOCK)
      II = II + NBLOCK
      CALL UCOPY (Q (LFIT + II), SS, NWW)
      IMQFUN = SS
      II = II + NWW
      CALL UCOPY (Q (LFIT + II), SS, NWW)
      VCONST = SS
      II = II + 4 *NWW
      DO 10    I = 1, NSIG
         CALL UCOPY (Q (LFIT + II), SS, NWW)
         SIGV (I, 1) = SS
         II = II + NWW
   10 CONTINUE
      IF (NDIM .GE. 2) THEN
         DO 20    I = 1, NSIG
            CALL UCOPY (Q (LFIT + II), SS, NWW)
            SIGV (I, 2) = SS
            II = II + NWW
   20    CONTINUE
      END IF
      IF (NDIM .GE. 3) THEN
         DO 30    I = 1, NSIG
            CALL UCOPY (Q (LFIT + II), SS, NWW)
            SIGV (I, 3) = SS
            II = II + NWW
   30    CONTINUE
      END IF
      IF (NDIM .GE. 4) THEN
         GO TO 60
      END IF
      DO 40    I = 1, NSIG
         CALL UCOPY (Q (LFIT + II), SS, NWW)
         SIGDEL (I) = SS
         II = II + NWW
   40 CONTINUE
 
      GO TO 70
 
   50 CONTINUE
      CALL HBUG ('Unrecognised structure', 'HQGETF', IDMQ)
      GO TO 70
 
   60 CONTINUE
      CALL HBUG ('Only programmed for 1- and 2-D', 'HQGETF', IDMQ)
      GO TO 70
 
   70 CONTINUE
 
      END
