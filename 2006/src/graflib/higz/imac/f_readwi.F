*
* $Id: f_readwi.F,v 1.1.1.1 1996/02/14 13:10:59 mclareni Exp $
*
* $Log: f_readwi.F,v $
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
        SUBROUTINE F_ReadWindowEvent(TheEvent)
c+
c       SUBROUTINE F_ReadWindowEvent(TheEvent)
c
c Someone else requested something similar to your event handling and we have
c an undocumented call that is described below.  Although I will put your entire
c commentary on events into the suggestions list so that the programmers can
c ponder your ideas.
c If you write a subroutine:
c       SUBROUTINE F_ReadWindowEvent(TheEvent)
c
c and put it in your link ahead of FORTRANlib, you will get a chance to see and
c steal every event that comes through during a READ from the output window.  If
c you handle the event you should set the .what field to 0 so the FORTRAN
c runtime code won't try to handle the event again.
c-
!!SETC USINGINCLUDES = .FALSE.
!!T72-
        include 'types.f'
        include 'Windows.f'
        include 'Events.f'
        include 'QDOffscreen.f'
        include 'Quickdraw.f'
        include 'globals.f'
c
        integer  OutputWindow
        external OutputWindow
c
        record /EventRecord/    TheEvent
        record /WindowPtr/      wptr
        record /GWorldptr/      oldGWorld
        record /GDHandle/       oldGDHandle
        record /CWindowPtr/     myCWindow
        record /GWorldptr/      myGWorld
        record /GDHandle/       mywGDHandle,mygGDHandle
        record /RGBcolor/       colorblack,colorwhite
        record /PixMapHandle/   myPixMapHandle
        record /grafport/       myGport
c       record /TEHandle/       myTEHandle
        logical jclgra,jfrmst,jmouse
        common /mpwevt/jclgra,jfrmst,jmouse,ixcur,iycur,
     #  myCWindow,myGWorld,mywGDHandle,mygGDHandle,colorblack,
     #  colorwhite,myGport
        integer*2 WindowPart
        logical jdone,jevdon,jlocal
        save
        data jdone/.false./
c
        if(.not.jdone)then
         call F_AboutString(
     #   '         Program and libraries',
     #   '             written by',
     #   '            C.Th. de Laat',
     #   'Physics department, Utrecht University',
     #   '   (all rights reserved 1988 - 1995)')
         call F_GetOutWindowSize(il,it,ir,ib)
         call MoveOutWindow(10,it,542,ib)
         call OutWindowScroll(9999)
         jdone=.true.
         jfrmst=.true.
        endif
        jevdon=.false.
c
        if(TheEvent.what.eq.nullEvent)then
        elseif(TheEvent.what.eq.mouseDown)then
         WindowPart = FINDWindow(TheEvent.Where,%ref(wptr))
         if(WindowPart.eq.inDesk)then
         elseif(WindowPart.eq.inMenuBar)then
cccc      if(myCWindow.P.eq.FrontWindow())jevdon=.true.
         elseif(WindowPart.eq.inSysWindow)then
          call SystemClick(theEvent,wptr)
          jevdon=.true.
         elseif(WindowPart.eq.inContent)then
          if(wptr.P .eq. myCWindow.P)then
           if(wptr.P.eq.FrontWindow())jmouse=.true.
           jevdon=.true.
          endif
          if(wptr.P.ne.FrontWindow())call SelectWindow(wptr.P)
         elseif(WindowPart.eq.inDrag)then
          if(wptr.P.ne.FrontWindow())call SelectWindow(wptr.P)
          if(wptr.P.eq.myCWindow.P)then
!!IFC NOT LSPOWERF
        QDG = JQDGLOBALS()
!!ELSEC
        QDG = %loc(qd)
!!ENDC
           call DragWindow(wptr,TheEvent.where,QDG^.screenbits.bounds)
           ier=UpdateGWorld(%ref(myGWorld.P),int2(0),
     #     myCWindow.P^.portrect,nil,nil,clipPix)
           jevdon=.true.
          endif
         elseif(WindowPart.eq.inGrow)then
         elseif(WindowPart.eq.inGoAway)then
          if(wptr.P .eq. OutputWindow() )then
           jlocal = TrackGoaway(wptr,TheEvent.where)
           if(jlocal.and.myCWindow.P.ne.nil)then
            call SelectWindow(myCWindow.P)
           endif
           jevdon=.true.
          elseif(wptr.P .eq. myCWindow.P)then
           jlocal = TrackGoaway(wptr,TheEvent.where)
           jclgra = jclgra.or.jlocal
           if(jlocal.and.OutputWindow().ne.nil)then
            call SelectWindow( %val(OutputWindow()) )
           endif
           jevdon=.true.
          endif
         endif
        elseif(TheEvent.what.eq.mouseUp)then
        elseif(TheEvent.what.eq.keyDown)then
        elseif(TheEvent.what.eq.keyUp)then
        elseif(TheEvent.what.eq.autoKey)then
        elseif(TheEvent.what.eq.updateEvt)then
         if(TheEvent.message.eq.myCWindow.P)then
          call BeginUpdate(TheEvent.message)
          call GetGWorld(%ref(oldGWorld),%ref(oldGDHandle))
          call SetGWorld(myCWindow,mywGDHandle)
          call RGBForeColor(colorblack)
          call RGBBackColor(colorwhite)
          ier=LockPixels(myGWorld.P^.portPixMap)
          myPixMapHandle.H=GetGWorldPixMap(myGWorld)
          call copybits(
     #        myPixMapHandle.H^.P^,myCWindow.P^.portPixMap.H^.P^,
     #        myGWorld.P^.portrect,myCWindow.P^.portrect,
     #        srccopy             ,myCWindow.P^.visrgn)
          call UnlockPixels(myGWorld.P^.portPixMap)
          ixcur=-11111
          iycur=-11111
          call SetGWorld(oldGWorld,oldGDHandle)
          call EndUpdate(TheEvent.message)
          jevdon=.true.
ccccc
c        elseif(TheEvent.message.eq.OutputWindow())then
c         call BeginUpdate(TheEvent.message)
c         call F_DRAWOUTPWINDOW
c         call DrawControls(TheEvent.message)
c         call DrawGrowIcon(TheEvent.message)
c         call EndUpdate(TheEvent.message)
c         jevdon=.true.
ccccc
         endif
        elseif(TheEvent.what.eq.diskEvt)then
        elseif(TheEvent.what.eq.activateEvt)then
        elseif(TheEvent.what.eq.networkEvt)then
        elseif(TheEvent.what.eq.driverEvt)then
        elseif(TheEvent.what.eq.app1Evt)then
        elseif(TheEvent.what.eq.app2Evt)then
        elseif(TheEvent.what.eq.app3Evt)then
        elseif(TheEvent.what.eq.osEvt)then
         if(iand(ishft(theEvent.message,-24),suspendResumeMessage).ne.0)
     #   jfrmst=iand(theEvent.message,resumeFlag).ne.0
        endif
        if(jevdon)TheEvent.what=0
        return
        end
#endif
