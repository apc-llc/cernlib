*
* $Id: ihmc03.F,v 1.1.1.1 1996/02/14 13:10:50 mclareni Exp $
*
* $Log: ihmc03.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:50  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.50.26  by  O.Couet
*-- Author :
      SUBROUTINE IHMC03(NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMC03                                     Date:    10.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Consider case No 3                                     *
*                                                                      *
*     Input: see common HCMCUB                                         *
*                                                                      *
*     Output: the same as for IHMCUB                                   *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      REAL              XYZ(3,*),GRAD(3,*)
      INTEGER           ITRIA(3,*),IE(6),IT1(3,2),IT2(3,4)
      DATA              IE/4,9,1, 2,11,3/
      DATA              IT1/1,2,3, 4,5,6/
      DATA              IT2/1,2,-5, -1,5,6, 5,-2,4, -4,2,3/
*-
**          S E T   N O D E S   &   N O R M A L E S
*
      NNOD   = 6
      CALL IHMCPP(NNOD,IE,XYZ,GRAD)
*
**          F I N D   C O N F I G U R A T I O N
*
      F0     = (F8(1)*F8(3)-F8(2)*F8(4)) / (F8(1)+F8(3)-F8(2)-F8(4))
      IF (F0.GE.0. .AND. F8(1).GE.0.)   GOTO 100
      IF (F0.LT.0. .AND. F8(1).LT.0.)   GOTO 100
      NTRIA  = 2
      CALL IHMCTT(NTRIA,IT1,ITRIA)
      GOTO 999
*
**          N O T   S E P A R A T E D   F R O N T   F A C E
*
  100 NTRIA  = 4
      CALL IHMCTT(NTRIA,IT2,ITRIA)
      GOTO 999
*
  999 RETURN
      END
