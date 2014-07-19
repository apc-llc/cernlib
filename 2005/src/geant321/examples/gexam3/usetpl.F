*
* $Id: usetpl.F,v 1.1.1.1 1995/10/24 10:22:13 cernlib Exp $
*
* $Log: usetpl.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:13  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE USETPL
C
#include "geant321/gclist.inc"
C
      CALL HBOOK1(100,' XHITS IN LFDR$',100,-50.,50.,0.)
      CALL HBOOK1(101,' YHITS IN LFDR$', 40,-20.,20.,0.)
      CALL HBOOK1(102,' ZHITS IN LFDR$', 20,-10.,10.,0.)
      CALL HBOOK1(110,' XHITS IN RFDR$',100,-50.,50.,0.)
      CALL HBOOK1(111,' YHITS IN RFDR$', 40,-20.,20.,0.)
      CALL HBOOK1(112,' ZHITS IN RFDR$', 20,-10.,10.,0.)
C
      RETURN
      END
