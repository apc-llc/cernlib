*
* $Id: gphsg1.F,v 1.2 1999/01/20 13:37:00 japost Exp $
*
* $Log: gphsg1.F,v $
* Revision 1.2  1999/01/20 13:37:00  japost
*   Changed
*       IPOINT = JPHXS+Q(JPHXS+1)*3+2
* to
*       IPOINT = JPHXS+INT(Q(JPHXS+1))*3+2
* to protect against large values that cannot be accurately treated as Reals
*
* > Date: Fri, 20 Nov 1998 17:31:34 -0600 (CST)
* > From: Shuichi Kunori <kunori@fsgi02.FNAL.GOV>
* >
* >We increased the GCBANK size from 12M words to 28M words and found a
* >problem in some G3 routines. For example, s/r GPHSG1 in gphys has a
* >line
* >
* >      IPOINT = JPHXS+Q(JPHXS+1)*3+2
* >
* >Since second term on the right has Q, the calculation is done in REAL.
* >If JPHYS is bigger than 16.7M (7 digits precision for real number),
* >the calculation overflows     [Actually it loses precision JApostolakis]
* >and produces meaningless address IPOINT.
* >Once we started using 28M for GCBANK, IPOINT was collapsed and producing
* >wrong cross section for photo effect. We found other cases similar to
* > this.
*
* Revision 1.1  1995/10/24 10:21:28  cernlib
* Initial revision
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.22  by  S.Giani
*-- Author :
      FUNCTION GPHSG1(E)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       FUNCTION TO COMPUTE THE PHOTOEFFECT TOTAL CROSS-SECTION  *
C.    *       AS A FUNCTION OF E                                       *
C.    *                                                                *
C.    *    ==>CALLED BY : GPHOTI                                       *
C.    *                                                                *
C.    *    (1) Sandia parametrizsation is used;                        *
C.    *    (2) GPHSG1 in cm**2/g.                                      *
C.    *    (3) E in GeV                                                *
C.    *                                                                *
C.    *     J. Chwastowski 17.11.92                                    *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gconsp.inc"
#include "geant321/gc10ev.inc"
#if !defined(CERNLIB_SINGLE)
      DOUBLE PRECISION EINV,ECUR,ONE
#endif
      PARAMETER (ONE=1)
C.
C.    ------------------------------------------------------------------
C.
      RES = 0.
      IF(E.LT.G10EV) GO TO 20
C Use Sandia data
      JPHXS = LQ(JPHOT-1)
      IPOINT = JPHXS+INT(Q(JPHXS+1))*3+2
      IMAX = Q(IPOINT)
      IPOINT = IPOINT+1
      ECUR = E*1.E6
      IF(ECUR.LT.Q(IPOINT)) GO TO 20
      EINV = ONE/ECUR
      DO 10 I = 2,IMAX
         IPOINT = IPOINT+5
         IF(ECUR.LT.Q(IPOINT)) THEN
            J = IPOINT+1
            RES = EINV*(Q(J)+EINV*(Q(J+1)+EINV*(Q(J+2)+EINV*Q(J+3))))
            GO TO 20
         ENDIF
   10 CONTINUE
C This value is in cm**2/g
   20 GPHSG1 = MAX(RES,0.)
C
      END
