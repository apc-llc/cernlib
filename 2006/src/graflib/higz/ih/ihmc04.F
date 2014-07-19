*
* $Id: ihmc04.F,v 1.1.1.1 1996/02/14 13:10:52 mclareni Exp $
*
* $Log: ihmc04.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:52  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.50.26  by  O.Couet
*-- Author :
      SUBROUTINE IHMC04(NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMC04                                     Date:    10.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Consider case No 4                                     *
*                                                                      *
*     Input: see common HCMCUB                                         *
*                                                                      *
*     Output: the same as for IHMCUB                                   *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      REAL              XYZ(3,*),GRAD(3,*)
      INTEGER           ITRIA(3,*),IE(6),IT1(3,2),IT2(3,6)
      DATA              IE/4,9,1, 7,11,6/
      DATA              IT1/1,2,3, 4,5,6/
      DATA              IT2/1,2,4, 2,3,6, 3,1,5, 4,5,1, 5,6,3, 6,4,2/
*-
**          S E T   N O D E S   &   N O R M A L E S
*
      NNOD   = 6
      CALL IHMCPP(NNOD,IE,XYZ,GRAD)
*
**          I S   T H E R E   S U R F A C E   P E N E T R A T I O N ?
*
      CALL IHMCX(F8(1),F8(2),F8(3),F8(4),F8(5),F8(6),F8(7),F8(8),IREP)
      IF (IREP .EQ. 0) THEN
        NTRIA  = 2
        CALL IHMCTT(NTRIA,IT1,ITRIA)
      ELSE
        NTRIA  = 6
        CALL IHMCTT(NTRIA,IT2,ITRIA)
      END IF
      RETURN
      END
