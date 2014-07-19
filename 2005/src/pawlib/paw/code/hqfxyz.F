*
* $Id: hqfxyz.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hqfxyz.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.15/00 25/08/92  18.38.21  by  Rene Brun
*-- Author :
      REAL FUNCTION HQFXYZ (X, Y, Z)
*     IMPLICIT NONE
      REAL X, Y, Z
* Determines sum of multiquadrics less isosurface value for FUNCTION/DRAW.
* X, Y, Z are plot coordinates.

#include "paw/pawnpu.inc"

      REAL V (3)
      REAL HQF

      V (1) = X
      V (2) = Y
      V (3) = Z

      HQFXYZ = HQF (V) - VALHQF

      END
