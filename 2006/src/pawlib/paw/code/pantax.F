*
* $Id: pantax.F,v 1.1.1.1 1996/03/01 11:38:42 mclareni Exp $
*
* $Log: pantax.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.13/00 10/02/92  15.22.13  by  O.Couet
*-- Author : G.Richards
      SUBROUTINE PANTAX(XAXIS,YAXIS,ZAXIS)
*********************************************************************
*     Draws the axis and sets up the scales
*********************************************************************
#include "paw/gplotst.inc"
      CHARACTER*(*) XAXIS,YAXIS,ZAXIS
      DIMENSION PXA(10),PYA(10),PZA(10)
      CHARACTER*6 NUMB
*
*              Create axis structure
*
      CALL IGQ('DIME',RDIM)
      IF (RDIM.EQ.3.) THEN
         CALL IGSTR(ISTCNT,'N')
         ISTCNT=ISTCNT+1
      ELSE
         RETURN
      ENDIF
      RMNX=RMINP(1)
      RMNY=RMINP(2)
      RMNZ=RMINP(3)
      RMXX=RMAXP(1)
      RMXY=RMAXP(2)
      RMXZ=RMAXP(3)
      PXA(1)=RMNX
      PYA(1)=RMNY
      PZA(1)=RMNZ
      PXA(2)=RMXX
      PYA(2)=RMNY
      PZA(2)=RMNZ
      PXA(3)=RMXX
      PYA(3)=RMXY
      PZA(3)=RMNZ
      PXA(4)=RMNX
      PYA(4)=RMXY
      PZA(4)=RMNZ
      PXA(5)=RMNX
      PYA(5)=RMNY
      PZA(5)=RMNZ
      N=5
      CALL IPL3(N,PXA,PYA,PZA)
      PZA(1)=RMXZ
      PZA(2)=RMXZ
      PZA(3)=RMXZ
      PZA(4)=RMXZ
      PZA(5)=RMXZ
      CALL IPL3(N,PXA,PYA,PZA)
      PXA(2)=RMNX
      PYA(2)=RMNY
      PZA(2)=RMNZ
      N=2
      CALL IPL3(N,PXA,PYA,PZA)
      PXA(1)=RMXX
      PYA(1)=RMNY
      PZA(1)=RMNZ
      PXA(2)=RMXX
      PYA(2)=RMNY
      PZA(2)=RMXZ
      CALL IPL3(N,PXA,PYA,PZA)
      PXA(1)=RMNX
      PYA(1)=RMXY
      PZA(1)=RMNZ
      PXA(2)=RMNX
      PYA(2)=RMXY
      PZA(2)=RMXZ
      CALL IPL3(N,PXA,PYA,PZA)
      PXA(1)=RMXX
      PYA(1)=RMXY
      PZA(1)=RMNZ
      PXA(2)=RMXX
      PYA(2)=RMXY
      PZA(2)=RMXZ
      CALL IPL3(N,PXA,PYA,PZA)
      STZ=(RMXZ-RMNZ)/10
      STY=(RMXY-RMNY)/10
      STX=(RMXX-RMNX)/10
      N=8
      HEIGHT=2.0
      RPX=RMXX+.2*STX
      RPY=RMNY-0.2*STY
      RPZ=RMNZ-0.2*STZ
      CALL IATR3(RPX,RPY,RPZ,0,0,0,XAXIS)
      RPX=RMNX-0.2*STX
      RPY=RMXY+.2*STY
      RPZ=RMNZ-0.2*STZ
      CALL IATR3(RPX,RPY,RPZ,0,0,0,YAXIS)
      RPX=RMNX-0.2*STX
      RPY=RMNY-0.2*STY
      RPZ=RMXZ+.2*STZ
      CALL IATR3(RPX,RPY,RPZ,0,0,0,ZAXIS)
      RPX=RMNX
      RPY=RMNY
      RPZ=RMXZ+1*STZ
      HEIGHT=3
      HEIGHT=2
      STEP=0.1
      DO 10 I=1,9
         N=3
         PXA(1)=RMNX+0.2*STX
         PYA(1)=RMNY
         PZA(1)=RMNZ+STZ*I
         PXA(2)=RMNX
         PYA(2)=RMNY
         PZA(2)=RMNZ+STZ*I
         PXA(3)=RMNX
         PYA(3)=RMNY+0.2*STY
         PZA(3)=RMNZ+STZ*I
         CALL IPL3(N,PXA,PYA,PZA)
         RPX=RMNX-.2*STX
         RPY=RMNY-.2*STY
         RPZ=RMNZ+STZ*I
         T= RMINP(3)+(RMAXP(3)-RMINP(3))*STEP*FLOAT(I)
         CALL IZRTOC(T,NUMB)
         N=10
         CALL IATR3(RPX,RPY,RPZ,0,0,0,NUMB)
         N=3
         PXA(1)=RMNX+STX*I
         PYA(1)=RMNY
         PZA(1)=RMNZ+0.2*STZ
         PXA(2)=RMNX+STX*I
         PYA(2)=RMNY
         PZA(2)=RMNZ
         PXA(3)=RMNX+STX*I
         PYA(3)=RMNY+0.2*STY
         PZA(3)=RMNZ
         CALL IPL3(N,PXA,PYA,PZA)
         RPX=RMNX+STX*I
         RPY=RMNY-.2*STY
         RPZ=RMNZ-.2*STX
         T= RMINP(1)+(RMAXP(1)-RMINP(1))*STEP*FLOAT(I)
         CALL IZRTOC(T,NUMB)
         N=10
         CALL IATR3(RPX,RPY,RPZ,0,0,0,NUMB)
         N=3
         PXA(1)=RMNX+0.2*STX
         PYA(1)=RMNY+STY*I
         PZA(1)=RMNZ
         PXA(2)=RMNX
         PYA(2)=RMNY+STY*I
         PZA(2)=RMNZ
         PXA(3)=RMNX
         PYA(3)=RMNY+STY*I
         PZA(3)=RMNZ+0.2*STZ
         CALL IPL3 (N,PXA,PYA,PZA)
         RPX=RMNX-.2*STX
         RPY=RMNY+STY*I
         RPZ=RMNZ-.2*STZ
         T= RMINP(2)+(RMAXP(2)-RMINP(2))*STEP*FLOAT(I)
         CALL IZRTOC(T,NUMB)
         N=10
         CALL IATR3(RPX,RPY,RPZ,0,0,0,NUMB)
   10 CONTINUE
      IF (RDIM.EQ.3.) THEN
         CALL IGSTR(ISTCNT-1,'C')
*
*              Reopen the top and make the instantiations
*
         CALL IGSTR(101,'O')
         CALL IGSTR(ISTCNT-1,'E')
         CALL IGSTR(101,'C')
      ENDIF
      END
