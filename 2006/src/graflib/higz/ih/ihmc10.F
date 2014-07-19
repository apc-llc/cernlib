*
* $Id: ihmc10.F,v 1.1.1.1 1996/02/14 13:10:52 mclareni Exp $
*
* $Log: ihmc10.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:52  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.50.26  by  O.Couet
*-- Author :
      SUBROUTINE IHMC10(NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMC10                                     Date:    10.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Consider case No 10                                    *
*                                                                      *
*     Input: see common HCMCUB                                         *
*                                                                      *
*     Output: the same as for IHMCUB                                   *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      REAL              XYZ(3,*),GRAD(3,*)
      INTEGER           ITRIA(3,*),IE(8),IT(3,8,6)
      DATA              IE/1,3,12,9, 5,7,11,10/
      DATA              IT/
     & 1,2,-3, -1,3,4, 5,6,-7, -5,7,8, 0,0,0,   0,0,0, 0,0,0,   0,0,0,
     & 9,1,2,   9,2,3, 9,3,4,   9,4,5, 9,5,6,   9,6,7, 9,7,8,   9,8,1,
     & 9,1,2,   9,4,1, 9,3,4,   9,6,3, 9,5,6,   9,8,5, 9,7,8,   9,2,7,
     & 1,2,-7, -1,7,8, 5,6,-3, -5,3,4, 0,0,0,   0,0,0, 0,0,0,   0,0,0,
     & 1,2,-7, -1,7,8, 2,3,-6, -2,6,7, 3,4,-5, -3,5,6, 4,1,-8, -4,8,5,
     & 1,2,-3, -1,3,4, 2,7,-6, -2,6,3, 7,8,-5, -7,5,6, 8,1,-4, -8,4,5/
*-
**          S E T   N O D E S   &   N O R M A L E S
*
      NNOD   = 8
      CALL IHMCPP(NNOD,IE,XYZ,GRAD)
*
**          F I N D   C O N F I G U R A T I O N
*
      F1     = (F8(1)*F8(6)-F8(2)*F8(5)) / (F8(1)+F8(6)-F8(2)-F8(5))
      F2     = (F8(4)*F8(7)-F8(3)*F8(8)) / (F8(4)+F8(7)-F8(3)-F8(8))
      ICASE  = 1
      IF (F1 .GE. 0.)                   ICASE = ICASE + 1
      IF (F2 .GE. 0.)                   ICASE = ICASE + 2
      IF (ICASE.EQ.1 .OR. ICASE.EQ.4)   GOTO 100
*
**         D I F F E R E N T    T O P   A N D   B O T T O M
*
      NNOD   = 9
      NTRIA  = 8
      CALL IHMCMP(8,XYZ,GRAD,IT(1,1,ICASE),XYZ(1,NNOD),GRAD(1,NNOD))
      GOTO 200
*
**          I S   T H E R E   S U R F A C E   P E N E T R A T I O N ?
*
  100 CALL IHMCX(F8(1),F8(2),F8(6),F8(5),F8(4),F8(3),F8(7),F8(8),IREP)
      NTRIA = 4
      IF (IREP .EQ. 0)                  GOTO 200
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
