*
* $Id: ihdfl1.F,v 1.1.1.1 1996/02/14 13:10:49 mclareni Exp $
*
* $Log: ihdfl1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:49  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 17/01/92  18.23.27  by  O.Couet
*-- Author :
      SUBROUTINE IHDFL1(ICODES,XYZ,NP,IFACE,TT)
************************************************************************
*                                                                      *
*     IHDFL1                                     Date:    10.01.92     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Draw face - 1st variant for "MOVING SCREEN" algorithm  *
*               (draw face with level lines)                           *
*                                                                      *
*     References: IHWLIN, IHWTON, IHSDRL, IHSMDF                       *
*                                                                      *
*     Input: ICODES(*) - set of codes for the line (not used)          *
*              ICODES(1) - IX                                          *
*              ICODES(2) - IY                                          *
*            XYZ(3,*)  - coordinates of nodes                          *
*            NP        - number of nodes                               *
*            IFACE(NP) - face                                          *
*            TT(NP)    - additional function defined on this face      *
*                        (not used in this routine)                    *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
#include "higz/hcline.inc"
      REAL              XYZ(3,*),P1(3),P2(3),P3(3,12),TT(*),X(2),Y(2)
      INTEGER           ICODES(*),IFACE(*)
*-
**          C O P Y   P O I N T S   T O   A R R A Y
*
      DO 100 I=1,NP
        K      = IFACE(I)
        P3(1,I) = XYZ(1,K)
        P3(2,I) = XYZ(2,K)
        P3(3,I) = XYZ(3,K)
  100   CONTINUE
*
**          F I N D   L E V E L   L I N E S
*
      CALL IHWLIN(NP,P3,TT)
*
**          D R A W   L E V E L   L I N E S
*
      CALL ISLN(3)
      DO 220 IL=1,NLINES
        CALL IHSDRL(PLINES(1,1,IL),PLINES(1,2,IL))
        CALL IHWTON(PLINES(1,1,IL),P1)
        CALL IHWTON(PLINES(1,2,IL),P2)
        XDEL   = P2(1) - P1(1)
        YDEL   = P2(2) - P1(2)
        DO 210 IT=1,NT
          X(1) = P1(1) + XDEL*T(1,IT)
          Y(1) = P1(2) + YDEL*T(1,IT)
          X(2) = P1(1) + XDEL*T(2,IT)
          Y(2) = P1(2) + YDEL*T(2,IT)
          CALL IPL(2,X,Y)
  210   CONTINUE
  220 CONTINUE
*
**          D R A W   F A C E
*
      CALL ISLN(1)
      DO 320 I=1,NP
        I1     = I
        I2     = I + 1
        IF (I .EQ. NP)           I2 = 1
        CALL IHSDRL(P3(1,I1),P3(1,I2))
        CALL IHWTON(P3(1,I1),P1)
        CALL IHWTON(P3(1,I2),P2)
        XDEL   = P2(1) - P1(1)
        YDEL   = P2(2) - P1(2)
        DO 310 IT=1,NT
          X(1) = P1(1) + XDEL*T(1,IT)
          Y(1) = P1(2) + YDEL*T(1,IT)
          X(2) = P1(1) + XDEL*T(2,IT)
          Y(2) = P1(2) + YDEL*T(2,IT)
          CALL IPL(2,X,Y)
  310   CONTINUE
  320 CONTINUE
*
**          M O D I F Y    S C R E E N
*
      DO 400 I=1,NP
        I1     = I
        I2     = I + 1
        IF (I .EQ. NP)           I2 = 1
        CALL IHSMDF(P3(1,I1),P3(1,I2))
  400 CONTINUE
*
      END
