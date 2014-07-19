*
* $Id: igsvie.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igsvie.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 24/11/95  09.57.08  by  O.Couet
*-- Author :    O.Couet   31/05/94
      SUBROUTINE IGSVIE
*.===========>
*.
*.   This routine computes the 3D transformation according to
*. the current 3D window and viewport. This routine is called
*. by ISELNT.
*.
*..==========> (O.Couet)
#include "higz/hi3d.inc"
#include "higz/hctnor.inc"
#if defined(CERNLIB_ZBUF)
#include "higz/hiatt.inc"
#endif
      DIMENSION RMIN(3),RMAX(3)
*.______________________________________
*
#if defined(CERNLIB_ZBUF)
#include "higz/hiwcdc.inc"
#endif
      CALL IGNG3D(W3X1,W3X2,W3Y1,W3Y2,W3Z1,W3Z2
     +,           PHI,THETA,PSI
     +,           V3X1,V3X2,V3Y1,V3Y2)
      RMIN(1) = W3X1
      RMIN(2) = W3Y1
      RMIN(3) = W3Z1
      RMAX(1) = W3X2
      RMAX(2) = W3Y2
      RMAX(3) = W3Z2
#if defined(CERNLIB_ZBUF)
      IF (IZBUF.NE.0) THEN
         IWKNB=1
         IZX = IXWCDC(V3X2)-IXWCDC(V3X1)
         IZY = IYWCDC(V3Y1)-IYWCDC(V3Y2)
         IZZ = MAX(IZX,IZY)
         CALL IHZINI(IZX,IZY,IZZ)
         CALL IHZWRD(V3X1,V3Y1,-1.,V3X2,V3Y2,1.)
         IFIRST = 1
      ENDIF
#endif
      CALL IHSVIE(RMIN,RMAX,PHI,THETA,PSI,IREP)
      COEFX   = (V3X2-V3X1)/2.
      AWNX    = COEFX*TNORM(1,1)
      BWNX    = COEFX*TNORM(2,1)
      CWNX    = COEFX*TNORM(3,1)
      DWNX    = COEFX+TNORM(4,1)+V3X1
      COEFY   = (V3Y2-V3Y1)/2.
      AWNY    = COEFY*TNORM(1,2)
      BWNY    = COEFY*TNORM(2,2)
      CWNY    = COEFY*TNORM(3,2)
      DWNY    = COEFY+TNORM(4,2)+V3Y1
      AWNZ    = TNORM(1,3)
      BWNZ    = TNORM(2,3)
      CWNZ    = TNORM(3,3)
      DWNZ    = TNORM(4,3)
*
      END
