*
* $Id: ihmc07.F,v 1.2 2002/05/16 15:29:27 couet Exp $
*
* $Log: ihmc07.F,v $
* Revision 1.2  2002/05/16 15:29:27  couet
* - During the conversion to C++ (for ROOT), two bugs have been found
*   (and fixed) in the fortran code.
*
* Revision 1.1.1.1  1996/02/14 13:10:52  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.50.26  by  O.Couet
*-- Author :
      SUBROUTINE IHMC07(NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMC07                                     Date:    13.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Consider case No 7                                     *
*                                                                      *
*     Input: see common HCMCUB                                         *
*                                                                      *
*     Output: the same as for IHMCUB                                   *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      REAL              XYZ(3,*),GRAD(3,*)
      INTEGER           ITRIA(3,*),IE(9),IT(3,9,9)
      DATA              IE/3,12,4, 1,10,2, 11,6,7/
      DATA              IT/
     & 1,2,3, 4,5,6, 7,8,9, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
     & 1,2,3, 4,9,-7, -4,7,6, 9,4,-5, -9,5,8, 0,0,0,0,0,0,0,0,0,0,0,0,
     & 4,5,6, 8,3,-1, -8,1,7, 3,8,-9, -3,9,2, 0,0,0,0,0,0,0,0,0,0,0,0,
     &-10,2,3, 10,3,-1, -10,1,7, 10,7,-6, -10,6,4,
     &                           10,4,-5, -10,5,8, 10,8,9, 10,9,-2,
     & 7,8,9, 2,5,-6, -2,6,1, 5,2,-3, -5,3,4, 0,0,0,0,0,0,0,0,0,0,0,0,
     &-10,1,2, 10,2,-3, -10,3,4, 10,4,5, 10,5,-8,
     &                          -10,8,9, 10,9,-7, -10,7,6, 10,6,-1,
     & 10,2,3, 10,3,-4, -10,4,5, 10,5,-6, -10,6,1,
     &                           10,1,-7, -10,7,8, 10,8,-9, -10,9,2,
     & 1,7,6, -4,2,3, -4,9,-2, -9,4,-5, -9,5,8, 0,0,0,0,0,0,0,0,0,0,0,0,
     &-1,9,2, 1,2,3, 1,3,-4, 6,-1,4, 6,4,5,
     &              6,-5,7, -7,5,8, 7,8,9, 7,-9,1/
*-
**          S E T   N O D E S   &   N O R M A L E S
*
      NNOD   = 9
      CALL IHMCPP(NNOD,IE,XYZ,GRAD)
*
**          F I N D   C O N F I G U R A T I O N
*
      F1     = (F8(3)*F8(6)-F8(2)*F8(7)) / (F8(3)+F8(6)-F8(2)-F8(7))
      F2     = (F8(3)*F8(8)-F8(4)*F8(7)) / (F8(3)+F8(8)-F8(4)-F8(7))
      F3     = (F8(3)*F8(1)-F8(2)*F8(4)) / (F8(3)+F8(1)-F8(2)-F8(4))
      ICASE  = 1
      IF (F1.GE.0. .AND. F8(3).LT.0.)   ICASE = ICASE + 1
      IF (F1.LT.0. .AND. F8(3).GE.0.)   ICASE = ICASE + 1
      IF (F2.GE.0. .AND. F8(3).LT.0.)   ICASE = ICASE + 2
      IF (F2.LT.0. .AND. F8(3).GE.0.)   ICASE = ICASE + 2
      IF (F3.GE.0. .AND. F8(3).LT.0.)   ICASE = ICASE + 4
      IF (F3.LT.0. .AND. F8(3).GE.0.)   ICASE = ICASE + 4
      NTRIA  = 5
      GOTO (100,400,400,200,400,200,200,300),ICASE
  100 NTRIA  = 3
      GOTO 400
*
**          F I N D   A D D I T I O N A L   P O I N T
*
  200 NNOD   = 10
      NTRIA  = 9
      CALL IHMCMP(9,XYZ,GRAD,IT(1,1,ICASE),XYZ(1,NNOD),GRAD(1,NNOD))
      GOTO 400
*
**          I S   T H E R E   S U R F A C E   P E N E T R A T I O N ?
*
  300 CALL IHMCX(F8(4),F8(3),F8(7),F8(8),F8(1),F8(2),F8(6),F8(5),IREP)
      IF (IREP .NE. 2)                  GOTO 400
CCC      CALL IHMCTT(NTRIA,IT8,ITRIA)
      NTRIA = 9
      ICASE = 9
*
**          S E T   T R I A N G L E S
*
  400 CALL IHMCTT(NTRIA,IT(1,1,ICASE),ITRIA)
      RETURN
      END
