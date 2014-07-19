*
* $Id: ihmc06.F,v 1.1.1.1 1996/02/14 13:10:52 mclareni Exp $
*
* $Log: ihmc06.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:52  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.50.26  by  O.Couet
*-- Author :
      SUBROUTINE IHMC06(NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMC06                                     Date:    10.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Consider case No 6                                     *
*                                                                      *
*     Input: see common HCMCUB                                         *
*                                                                      *
*     Output: the same as for IHMCUB                                   *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      REAL              XYZ(3,*),GRAD(3,*)
      INTEGER           ITRIA(3,*),IE(7),IT1(3,5),IT2(3,3),IT3(3,7)
      DATA              IE/2,4,9,10, 6,7,11/
      DATA              IT1/6,7,-1, -6,1,2, 6,2,3, 6,3,-4, -6,4,5/
      DATA              IT2/1,2,-3, -1,3,4, 5,6,7/
      DATA              IT3/6,7,-1, -6,1,2, 6,2,3, 6,3,-4, -6,4,5,
     &                      1,7,-5, -1,5,4/
*-
**          S E T   N O D E S   &   N O R M A L E S
*
      NNOD   = 7
      CALL IHMCPP(NNOD,IE,XYZ,GRAD)
*
**          F I N D   C O N F I G U R A T I O N
*
      F0     = (F8(2)*F8(7)-F8(6)*F8(3)) / (F8(2)+F8(7)-F8(6)-F8(3))
      IF (F0.GE.0. .AND. F8(2).GE.0.)   GOTO 100
      IF (F0.LT.0. .AND. F8(2).LT.0.)   GOTO 100
*
**          I S   T H E R E   S U R F A C E   P E N E T R A T I O N ?
*
      CALL IHMCX(F8(3),F8(2),F8(6),F8(7),F8(4),F8(1),F8(5),F8(8),IREP)
      IF (IREP .EQ. 1) THEN
        NTRIA = 7
        CALL IHMCTT(NTRIA,IT3,ITRIA)
      ELSE
        NTRIA = 3
        CALL IHMCTT(NTRIA,IT2,ITRIA)
      END IF
      GOTO 999
*
**          N O T   S E P A R A T E D   R I G T H   F A C E
*
  100 NTRIA  = 5
      CALL IHMCTT(NTRIA,IT1,ITRIA)
      GOTO 999
*
  999 RETURN
      END
