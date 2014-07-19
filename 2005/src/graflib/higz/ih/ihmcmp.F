*
* $Id: ihmcmp.F,v 1.1.1.1 1996/02/14 13:10:52 mclareni Exp $
*
* $Log: ihmcmp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:52  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.50.26  by  O.Couet
*-- Author :
      SUBROUTINE IHMCMP(NNOD,XYZ,GRAD,IT,PXYZ,PGRAD)
************************************************************************
*                                                                      *
*     IHMCMP                                     Date:    15.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Find meadle point of a polygon                         *
*                                                                      *
*     Input: NNOD      - number of nodes in the polygon                *
*            XYZ(3,*)  - node coordinates                              *
*            GRAD(3,*) - node normalies                                *
*            IT(3,*)   - division of the polygons into triangles       *
*                                                                      *
*     Output: PXYZ(3)  - meadle point coordinates                      *
*             PGRAD(3) - meadle point normale                          *
*                                                                      *
************************************************************************
      REAL              XYZ(3,*),GRAD(3,*),PXYZ(3),PGRAD(3),P(3),G(3)
      INTEGER           IT(3,*)
*-
      DO 100 I=1,3
        P(I)   = 0.
        G(I)   = 0.
  100 CONTINUE
      DO 220 N=1,NNOD
        K      = IT(3,N)
        IF (K .LT. 0)   K =-K
        DO 210 I=1,3
          P(I)  = P(I) + XYZ(I,K)
          G(I)  = G(I) + GRAD(I,K)
  210   CONTINUE
  220 CONTINUE
      DO 300 I=1,3
        PXYZ(I)  = P(I) / NNOD
        PGRAD(I) = G(I) / NNOD
  300 CONTINUE
      RETURN
      END
