*
* $Id: ihmcpp.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihmcpp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.50.26  by  O.Couet
*-- Author :
      SUBROUTINE IHMCPP(NNOD,IE,XYZ,GRAD)
************************************************************************
*                                                                      *
*     IHMCPP                                     Date:    15.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Find nodes and normales                                *
*                                                                      *
*     Input: NNOD  - number of nodes                                   *
*            IE(*) - edges which have section node                     *
*                                                                      *
*     Output: XYZ(3,*)  - nodes                                        *
*             GRAD(3,*) - node normales (not normalized)               *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      REAL              XYZ(3,*),GRAD(3,*)
      INTEGER           IE(*),IEDGE(2,12)
      DATA              IEDGE/
     & 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8, 8,5, 1,5, 2,6, 3,7, 4,8/
*-
      DO 200 N=1,NNOD
        K      = IE(N)
        IF (K .LT. 0)           K =-K
        N1     = IEDGE(1,K)
        N2     = IEDGE(2,K)
        T      = F8(N1) / (F8(N1)-F8(N2))
        DO 100 I=1,3
          XYZ(I,N)  = (P8(I,N2)-P8(I,N1))*T + P8(I,N1)
          GRAD(I,N) = (G8(I,N2)-G8(I,N1))*T + G8(I,N1)
  100   CONTINUE
  200 CONTINUE
      RETURN
      END
