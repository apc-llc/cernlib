*
* $Id: f_doback.F,v 1.1.1.1 1996/02/14 13:10:59 mclareni Exp $
*
* $Log: f_doback.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:59  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.23/06 01/11/95  14.04.54  by  O.Couet
*-- Author : Cees de Laat
!!IFC NOT LSPOWERF
!!mp 68kinlines
!!ELSEC
!!mp PPCinlines
!!ENDC
        subroutine F_DoBackground
!!SETC USINGINCLUDES = .FALSE.
!!T72-
        include 'Events.f'
        record /EventRecord/TheEvent
        logical*4 F_BACKEVENT
        external F_BACKEVENT
        logical jev
        integer*2 mask
        integer*4 mySleep
c
        mask=everyEvent-keyDownMask
        mySleep=0
100     if(WaitNextEvent(mask,%ref(TheEvent),mySleep,nil))then
         call F_ReadWindowEvent(TheEvent)
         if(TheEvent.what.ne.0)jev=F_BACKEVENT(%ref(TheEvent))
         goto 100
        endif
c
        return
        end
#endif
