*
* $Id: ihmc00.F,v 1.1.1.1 1996/02/14 13:10:50 mclareni Exp $
*
* $Log: ihmc00.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:50  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/06 27/08/93  13.53.33  by  O.Couet
*-- Author :
      SUBROUTINE IHMC00(K1,K2,K3,K4,K5,K6,NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMC00                                     Date:    20.07.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Consideration of trivial cases: 1,2,5,8,9,11,14        *
*                                                                      *
*     Input: K1,...,K6 - edges intersected with isosurface             *
*                                                                      *
*     Output: the same as for IHMCUB                                   *
*                                                                      *
************************************************************************
      REAL              XYZ(3,*),GRAD(3,*)
      INTEGER           ITRIA(3,*),IE(6),IT(3,4,4)
      DATA              IT/1,2,3,   0,0,0,   0,0,0,  0,0,0,
     &                     1,2,-3, -1,3,4,   0,0,0,  0,0,0,
     &                     1,2,-3, -1,3,-4, -1,4,5,  0,0,0,
     &                     1,2,-3, -1,3,-4, -4,6,-1, 4,5,-6/
*-
**          S E T   N O D E S   &   N O R M A L E S
*
      IE(1)  = K1
      IE(2)  = K2
      IE(3)  = K3
      IE(4)  = K4
      IE(5)  = K5
      IE(6)  = K6
      NNOD   = 6
      IF (IE(6) .EQ. 0)          NNOD = 5
      IF (IE(5) .EQ. 0)          NNOD = 4
      IF (IE(4) .EQ. 0)          NNOD = 3
      CALL IHMCPP(NNOD,IE,XYZ,GRAD)
*
**         S E T   T R I A N G L E S
*
      NTRIA  = NNOD - 2
      CALL IHMCTT(NTRIA,IT(1,1,NTRIA),ITRIA)
      RETURN
      END
