*
* $Id: ihwton.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwton.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/07 28/06/94  13.57.20  by  O.Couet
*-- Author :
      SUBROUTINE IHWTON(PW,PN)
************************************************************************
*                                                                      *
*     IHWTON                                     Date:    14.10.91     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Transfer point from world to normalized coordinates    *
*                                                                      *
*     Input: PW(3) - point in world coordinate system                  *
*            PN(3) - point in normalized coordinate system             *
*                                                                      *
************************************************************************
#include "higz/hctnor.inc"
      REAL PW(3),PN(3)
*-
      X      = PW(1)
      Y      = PW(2)
      Z      = PW(3)
      PN(1)  = TNORM(1,1)*X + TNORM(2,1)*Y + TNORM(3,1)*Z + TNORM(4,1)
      PN(2)  = TNORM(1,2)*X + TNORM(2,2)*Y + TNORM(3,2)*Z + TNORM(4,2)
      PN(3)  = TNORM(1,3)*X + TNORM(2,3)*Y + TNORM(3,3)*Z + TNORM(4,3)
      END
