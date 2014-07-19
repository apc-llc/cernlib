*
* $Id: ihbbox.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: ihbbox.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.15/02 17/07/92  13.45.04  by  O.Couet
*-- Author :
      SUBROUTINE IHBBOX(RMIN,RMAX,ANG,DRFACE)
************************************************************************
*                                                                      *
*     IHBBOX                                     Date:    13.01.92     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Draw back surfaces of surrounding box                  *
*                                                                      *
*     References: IHAXIV                                               *
*                                                                      *
*     Input: RMIN(3) - MIN -                                           *
*            RMAX(3) -       MAX scope                                 *
*            ANG     - angle between X and Y axis                      *
*                                                                      *
*            DRFACE(ICODES,XYZ,NP,IFACE,T) - routine for face drawing  *
*              ICODES(*) - set of codes for this face                  *
*              NP        - number of nodes in face                     *
*              IFACE(NP) - face                                        *
*              T(NP)     - additional function                         *
*                                                                      *
************************************************************************
      REAL              RMIN(3),RMAX(3),AV(3,8),R(3,8),TT(4)
      DOUBLE PRECISION  RAD,COSA,SINA
      INTEGER           ICODES(3),IFACE1(4),IFACE2(4)
      EXTERNAL          DRFACE
      DATA              IFACE1/1,4,8,5/,IFACE2/4,3,7,8/
*-
      RAD    = ATAN(1.D0)*4./180.
      COSA   = COS(RAD*ANG)
      SINA   = SIN(RAD*ANG)
      CALL IHAXIV(RMIN,RMAX,ANG,AV,IX1,IX2,IY1,IY2,IZ1,IZ2)
      DO 10 I=1,8
        R(1,I) = AV(1,I) + AV(2,I)*COSA
        R(2,I) = AV(2,I)*SINA
        R(3,I) = AV(3,I)
  10    CONTINUE
*
**          D R A W   F O R W A R D   F A C E S
*
      ICODES(1) = 0
      ICODES(2) = 0
      ICODES(3) = 0
      TT(1)  = R(3,IFACE1(1))
      TT(2)  = R(3,IFACE1(2))
      TT(3)  = R(3,IFACE1(3))
      TT(4)  = R(3,IFACE1(4))
      CALL DRFACE(ICODES,R,4,IFACE1,TT)
      TT(1)  = R(3,IFACE2(1))
      TT(2)  = R(3,IFACE2(2))
      TT(3)  = R(3,IFACE2(3))
      TT(4)  = R(3,IFACE2(4))
      CALL DRFACE(ICODES,R,4,IFACE2,TT)
*
      RETURN
      END
