*
* $Id: hqgiv.F,v 1.1.1.1 1996/01/16 17:08:02 mclareni Exp $
*
* $Log: hqgiv.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:02  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
      SUBROUTINE HQGIV (ND, NC, RMIN, RMAX)
      INTEGER ND, NC (*)
      REAL RMIN (*), RMAX (*)
* On input ND is dimension of NC, RMIN, RMAX.
* On output, it returns the following parameters after a multiquadric fit:
*   ND       = no. of dimensions.
*   NC (1)   = no. of channels in MQ histogram in x coordinate.
*   NC (2)   = no. of channels in MQ histogram in y coordinate.
*     etc.
*   RMIN (1) = minimum of x range.
*   RMIN (2) = minimum of y range.
*     etc.
*   RMAX (1) = maximum of x range.
*   RMAX (2) = maximum of y range.
*     etc.
* If ND < NDIM, only ND elements are filled but ND is set equal to NDIM.
 
#include "hbook/hcqcom.inc"
 
      INTEGER I, N
 
      N = MIN (ND, NDIM)
      DO 10    I = 1, N
         NC   (I) = NBINS  (I)
         RMIN (I) = SIGVMI (I)
         RMAX (I) = SIGVMA (I)
   10 CONTINUE
      ND = NDIM
 
      END
