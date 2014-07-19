*
* $Id: ihwwnn.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwwnn.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 17/01/92  17.09.53  by  O.Couet
*-- Author :
      SUBROUTINE IHWWNN(PW,PN)
************************************************************************
*                                                                      *
*     IHWWNN                                     Date:    17.01.92     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Transfer vector of NORMAL from word to normalized      *
*               coodinates                                             *
*                                                                      *
*     Input: PW(3) - vector of NORMAL in word coordinate system        *
*            PN(3) - vector of NORMAL in normalized coordinate system  *
*                                                                      *
************************************************************************
#include "higz/hctnor.inc"
      REAL              PW(3),PN(3)
*-
      X      = PW(1)
      Y      = PW(2)
      Z      = PW(3)
      A1     = TNORM(1,1)
      A2     = TNORM(2,1)
      A3     = TNORM(3,1)
      B1     = TNORM(1,2)
      B2     = TNORM(2,2)
      B3     = TNORM(3,2)
      C1     = TNORM(1,3)
      C2     = TNORM(2,3)
      C3     = TNORM(3,3)
      PN(1)  = X*(B2*C3-B3*C2) + Y*(B3*C1-B1*C3) + Z*(B1*C2-B2*C1)
      PN(2)  = X*(C2*A3-C3*A2) + Y*(C3*A1-C1*A3) + Z*(C1*A2-C2*A1)
      PN(3)  = X*(A2*B3-A3*B2) + Y*(A3*B1-A1*B3) + Z*(A1*B2-A2*B1)
      RETURN
      END

