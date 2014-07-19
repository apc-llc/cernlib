*
* $Id: ihmc13.F,v 1.1.1.1 1996/02/14 13:10:52 mclareni Exp $
*
* $Log: ihmc13.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:52  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.50.26  by  O.Couet
*-- Author :
      SUBROUTINE IHMC13(NNOD,NTRIA,XYZ,GRAD,ITRIA)
************************************************************************
*                                                                      *
*     IHMC13                                     Date:    13.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Consider case No 13                                    *
*                                                                      *
*     Input: see common HCMCUB                                         *
*                                                                      *
*     Output: the same as for IHMCUB                                   *
*                                                                      *
************************************************************************
#include "higz/hcmcub.inc"
      REAL              XYZ(3,*),GRAD(3,*),FF(8)
      INTEGER           ITRIA(3,*)
      INTEGER           IE(12),IFACE(4,6),IROTA(8,12),IWHAT(8)
      INTEGER           IT1(3,4), IT2(3,4), IT3(3,6), IT4(3,6)
      INTEGER           IT5(3,10),IT6(3,10),IT7(3,12)
      INTEGER           IT8(3,6), IT9(3,10),IT10(3,10)
      DATA              IROTA/
     & 1,2,3,4,5,6,7,8, 1,5,6,2,4,8,7,3, 1,4,8,5,2,3,7,6,
     & 3,7,8,4,2,6,5,1, 3,2,6,7,4,1,5,8, 3,4,1,2,7,8,5,6,
     & 6,7,3,2,5,8,4,1, 6,5,8,7,2,1,4,3, 6,2,1,5,7,3,4,8,
     & 8,4,3,7,5,1,2,6, 8,5,1,4,7,6,2,3, 8,7,6,5,4,3,2,1/
      DATA              IWHAT/63,62,54,26,50,9,1,0/
      DATA              IE/1,2,3,4,5,6,7,8,9,10,11,12/
      DATA              IFACE/
     & 1,2,3,4, 5,6,7,8, 1,2,6,5, 2,6,7,3, 4,3,7,8, 1,5,8,4/
      DATA              IT1/1,2,10, 9,5,8, 6,11,7, 3,4,12/
      DATA              IT2/5,6,10, 1,4,9, 2,11,3, 7,8,12/
      DATA              IT3/10,12,-3, -10,3,2, 12,10,-1, -12,1,4,
     &                      9,5,8, 6,11,7/
      DATA              IT4/11,9,-1, -11,1,2, 9,11,-3, -9,3,4,
     &                      5,6,10, 7,8,12/
      DATA              IT5/13,2,-11, -13,11,7, 13,7,-6, -13,6,10,
     & 13,10,1, 13,1,-4, -13,4,12, 13,12,-3, -13,3,2, 5,8,9/
      DATA              IT6/13,2,-10, -13,10,5, 13,5,-6, -13,6,11,
     & 13,11,3, 13,3,-4, -13,4,9, 13,9,-1, -13,1,2, 12,7,8/
      DATA              IT7/13,2,-11, -13,11,7, 13,7,-6,  -13,6,10,
     &                      13,10,-5, -13,5,8,  13,8,-9,  -13,9,1,
     &                      13,1,-4,  -13,4,12, 13,12,-3, -13,3,2/
      DATA              IT8/-3,8,12, 3,-2,-8, -2,5,-8, 2,10,-5,
     &                       7,6,11, 1,4,9/
      DATA              IT9/7,12,-3, -7,3,11, 11,3,2, 6,11,-2, -6,2,10,
     & 6,10,5, 7,6,-5, -7,5,8, 7,8,12, 1,4,9/
      DATA              IT10/9,1,-10, -9,10,5, 9,5,8, 4,9,-8, -4,8,12,
     & 4,12,3, 1,4,-3, -1,3,2, 1,2,10, 7,6,11/
*-
      NNOD   = 0
      NTRIA  = 0
*
**         F I N D   C O N F I G U R A T I O N   T Y P E
*
      DO 130 NR=1,12
        K      = 0
        INCR   = 1
        DO 110 NF=1,6
          F1     = F8(IROTA(IFACE(1,NF),NR))
          F2     = F8(IROTA(IFACE(2,NF),NR))
          F3     = F8(IROTA(IFACE(3,NF),NR))
          F4     = F8(IROTA(IFACE(4,NF),NR))
          IF ((F1*F3-F2*F4)/(F1+F3-F2-F4) .GE. 0.)      K = K + INCR
          INCR   = INCR + INCR
  110   CONTINUE
        DO 120 I=1,8
          IF (K .NE. IWHAT(I))          GOTO 120
          ICASE  = I
          KR     = NR
          GOTO 200
  120   CONTINUE
  130 CONTINUE
      WRITE(*,*) 'IHMC13: configuration is not found'
      GOTO 999
*
**          R O T A T E   C U B E
*
  200 IF (ICASE.EQ.1 .OR. ICASE.EQ.8)   GOTO 300
      DO 220 N=1,8
        K      = IROTA(N,KR)
        FF(N)  = F8(K)
        DO 210 I=1,3
          XYZ(I,N)  = P8(I,K)
          GRAD(I,N) = G8(I,K)
  210   CONTINUE
  220 CONTINUE
      DO 240 N=1,8
        F8(N)  = FF(N)
        DO 230 I=1,3
          P8(I,N) = XYZ(I,N)
          G8(I,N) = GRAD(I,N)
  230   CONTINUE
  240 CONTINUE
*
**          S E T   N O D E S   &   N O R M A L E S
*
 300  NNOD   = 12
      CALL IHMCPP(NNOD,IE,XYZ,GRAD)
*
**          V A R I O U S   C O N F I G U R A T I O N S
*
      GOTO (410,430,450,480,470,460,440,420), ICASE
  410 NTRIA  = 4
      CALL IHMCTT(NTRIA,IT1,ITRIA)
      GOTO 999
  420 NTRIA  = 4
      CALL IHMCTT(NTRIA,IT2,ITRIA)
      GOTO 999
  430 NTRIA  = 6
      CALL IHMCTT(NTRIA,IT3,ITRIA)
      GOTO 999
  440 NTRIA  = 6
      CALL IHMCTT(NTRIA,IT4,ITRIA)
      GOTO 999
  450 NNOD   = 13
      NTRIA  = 10
      CALL IHMCMP(9,XYZ,GRAD,IT5,XYZ(1,NNOD),GRAD(1,NNOD))
      CALL IHMCTT(NTRIA,IT5,ITRIA)
      GOTO 999
  460 NNOD   = 13
      NTRIA  = 10
      CALL IHMCMP(9,XYZ,GRAD,IT6,XYZ(1,NNOD),GRAD(1,NNOD))
      CALL IHMCTT(NTRIA,IT6,ITRIA)
      GOTO 999
  470 NNOD   = 13
      NTRIA  = 12
      CALL IHMCMP(12,XYZ,GRAD,IT7,XYZ(1,NNOD),GRAD(1,NNOD))
      CALL IHMCTT(NTRIA,IT7,ITRIA)
      GOTO 999
**          I S   T H E R E   S U R F A C E   P E N E T R A T I O N ?
  480 CALL IHMCX(F8(3),F8(4),F8(1),F8(2),F8(7),F8(8),F8(5),F8(6),IREP)
      GOTO (481,482,483), IREP+1
  481 NTRIA  = 6
      CALL IHMCTT(NTRIA,IT8,ITRIA)
      GOTO 999
  482 NTRIA  = 10
      CALL IHMCTT(NTRIA,IT9,ITRIA)
      GOTO 999
  483 NTRIA  = 10
      CALL IHMCTT(NTRIA,IT10,ITRIA)
      GOTO 999
*
  999 RETURN
      END
