*
* $Id: ihwzn.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwzn.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 20/04/93  14.20.34  by  O.Couet
*-- Author :
      SUBROUTINE IHWZN(X,Y,Z,ZN)
************************************************************************
*                                                                      *
*     IHWZN                                      Date:    20.04.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Find Z component of NORMAL in normalized coordinates   *
*                                                                      *
*     Input: X - X-component of NORMAL                                 *
*            Y - Y-component of NORMAL                                 *
*            Z - Z-component of NORMAL                                 *
*                                                                      *
*     Output: ZN - Z-component of NORMAL in normalized coordinates     *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
*-
      ZN     = X*(TN(2,1)*TN(3,2)-TN(3,1)*TN(2,2)) +
     &         Y*(TN(3,1)*TN(1,2)-TN(1,1)*TN(3,2)) +
     &         Z*(TN(1,1)*TN(2,2)-TN(2,1)*TN(1,2))
      RETURN
      END
