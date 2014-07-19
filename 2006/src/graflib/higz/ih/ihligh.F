*
* $Id: ihligh.F,v 1.1.1.1 1996/02/14 13:10:50 mclareni Exp $
*
* $Log: ihligh.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:50  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 14/01/92  14.01.24  by  O.Couet
*-- Author :
      SUBROUTINE IHLIGH(NL,YL,XSCR,YSCR,ZSCR,IREP)
************************************************************************
*                                                                      *
*     IHLIGH                                     Date:    11.10.91     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Set light source                                       *
*                                                                      *
*     Input: NL   - source number: -1 off all light sources            *
*                                   0 set diffused light               *
*            YL   - intensity of the light source                      *
*            XSCR \                                                    *
*            YSCR  - direction of the light (in respect of the screen) *
*            ZSCR /                                                    *
*                                                                      *
*     Output: IREP   - reply : 0 - O.K.                                *
*                             -1 - error in light sources definition:  *
*                                  negative intensity                  *
*                                  source number greater than max      *
*                                  light source is placed at origin    *
*                                                                      *
************************************************************************
#include "higz/hcligh.inc"
*-
      IREP   = 0
      IF (NL) 100,200,300
**          S W I T C H   O F F   L I G H T S
  100 LOFF   = 1
      YDL    = 0.
      DO 110 I=1,LLIGHT
        YLS(I) = 0.
  110 CONTINUE
      GOTO 999
**          S E T   D I F F U S E D   L I G H T
  200 IF (YL .LT. 0.)                   GOTO 996
      YDL    = YL
      GOTO 400
**          S E T   L I G H T   S O U R C E
  300 IF (NL .GT. LLIGHT)               GOTO 997
      IF (YL .LT. 0.)                   GOTO 997
      S      = SQRT(XSCR*XSCR+YSCR*YSCR+ZSCR*ZSCR)
      IF (S .EQ. 0.)                    GOTO 998
      YLS(NL) = YL
      VLS(1,NL) = XSCR/S
      VLS(2,NL) = YSCR/S
      VLS(3,NL) = ZSCR/S
*           C H E C K   L I G H T S
  400 LOFF   = 0
      IF (YDL .NE. 0)                   GOTO 999
      DO 410 I=1,LLIGHT
        IF (YLS(I) .NE. 0.)             GOTO 999
  410 CONTINUE
      LOFF   = 1
      GOTO 999
*
  996 WRITE(*,*) 'IHLIGH: negative light intensity'
      IREP   =-1
      GOTO 999
  997 WRITE(*,*) 'IHLIGH: illegal light source number'
      IREP   =-1
      GOTO 999
  998 WRITE(*,*) 'IHLIGH: light source is placed at origin'
      IREP   =-1
      GOTO 999
  999 RETURN
      END
