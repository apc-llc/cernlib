*
* $Id: ihmc12.F,v 1.2 2002/05/16 15:29:28 couet Exp $
*
* $Log: ihmc12.F,v $
* Revision 1.2  2002/05/16 15:29:28  couet
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
      SUBROUTINE IHMC12(NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMC12                                     Date:    10.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Consider case No 12                                    *
*                                                                      *
*     Input: see common HCMCUB                                         *
*                                                                      *
*     Output: the same as for IHMCUB                                   *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      REAL              XYZ(3,*),GRAD(3,*)
      INTEGER           ITRIA(3,*),IE(8),IT(3,8,6)
      DATA              IE/3,12,4, 1,9,8,6,2/
      DATA              IT/
     & 1,2,3,  4,5,-6, -4,6,8,  6,7,8,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
     &-9,1,2,  9,2,-3, -9,3,4, 9,4,-5, -9,5,6, 9,6,-7, -9,7,8, 9,8,-1,
     &9,1,-2,  -9,2,6, 9,6,-7, -9,7,8, 9,8,-4, -9,4,5, 9,5,-3, -9,3,1,
     & 3,4,5,  1,2,-6, -1,6,8,  6,7,8,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
     & 7,8,6,  6,8,-1, -6,1,2, 3,1,-8, -3,8,4,  3,4,5, 3,5,-6, -3,6,2,
     & 7,8,6,  6,8,-4, -6,4,5, 3,4,-8, -3,8,1,  3,1,2, 3,2,-6, -3,6,5/
*-
**          S E T   N O D E S   &   N O R M A L E S
*
      NNOD   = 8
      CALL IHMCPP(NNOD,IE,XYZ,GRAD)
*
**          F I N D   C O N F I G U R A T I O N
*
      F1     = (F8(1)*F8(3)-F8(2)*F8(4)) / (F8(1)+F8(3)-F8(2)-F8(4))
      F2     = (F8(1)*F8(8)-F8(4)*F8(5)) / (F8(1)+F8(8)-F8(4)-F8(5))
      ICASE  = 1
      IF (F1 .GE. 0.)                   ICASE = ICASE + 1
      IF (F2 .GE. 0.)                   ICASE = ICASE + 2
      IF (ICASE.EQ.1 .OR. ICASE.EQ.4)   GOTO 100
*
**          F I N D   A D D I T I O N A L   P O I N T
*
      NNOD   = 9
      NTRIA  = 8
      CALL IHMCMP(8,XYZ,GRAD,IT(1,1,ICASE),XYZ(1,NNOD),GRAD(1,NNOD))
      GOTO 200
*
**          I S   T H E R E   S U R F A C E   P E N E T R A T I O N ?
*
  100 CALL IHMCX(F8(1),F8(2),F8(3),F8(4),F8(5),F8(6),F8(7),F8(8),IREP)
      NTRIA  = 4
      IF (IREP .NE. 1)                  GOTO 200
**          "B O T T L E   N E C K"
      NTRIA = 8
      IF (ICASE .EQ. 1)                 ICASE = 5
      IF (ICASE .EQ. 4)                 ICASE = 6
*
**          S E T   T R I A N G L E S
*
  200 CALL IHMCTT(NTRIA,IT(1,1,ICASE),ITRIA)
      RETURN
      END
