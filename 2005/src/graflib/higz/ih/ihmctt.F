*
* $Id: ihmctt.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihmctt.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/06 27/08/93  13.53.33  by  O.Couet
*-- Author :
      SUBROUTINE IHMCTT(NTRIA,IT,ITRIA)
************************************************************************
*                                                                      *
*     IHMCTT                                     Date:    15.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Set triangles                                          *
*               (if parameter IALL=1, all edges will be visible)       *
*                                                                      *
*     Input: NTRIA   - number of triagles                              *
*            IT(3,*) - triangles                                       *
*                                                                      *
*     Output: ITRIA(3,*) - triangles                                   *
*                                                                      *
************************************************************************
      INTEGER           IT(3,*),ITRIA(3,*)
*-
      DO 200 N=1,NTRIA
        DO 100 I=1,3
          K      = IT(I,N)
*         IF (K .LT. 0)         K =-K
          ITRIA(I,N) = K
  100   CONTINUE
  200 CONTINUE
      RETURN
      END
