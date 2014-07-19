*
* $Id: imacplo.F,v 1.1.1.1 1996/02/14 13:10:59 mclareni Exp $
*
* $Log: imacplo.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:59  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.23/06 01/11/95  14.04.53  by  O.Couet
*-- Author : Cees de Laat
!!IFC NOT LSPOWERF
!!mp 68kinlines
!!ELSEC
!!mp PPCinlines
!!ENDC
        subroutine  macplo(ix,iy,ip)
        entry      imacplo(ix,iy,ip)
c+
c       subroutine  macplo(ix,iy,ip)
c       entry      imacplo(ix,iy,ip)
c
c device plot routine for Macintosh screen in MPW-LS-fortran window.
c This routine is also usable as interface under the CERN-higz package.
c
c Normally ix and iy are integer plot coordinates, they should range
c from 0 inclusive to some upper bound in both directions.
c The point (0,0) should be at the left bottom corner, the positive
c x axis to the right and the positive y axis is upwards.
c
c ip is the function code (those preceded by * are supported):
c*ip =  999     close plot, it is ready
c*ip = 31       give back type of device
c               ix is device, iy = 1 = screen with cursor readout,
c                             iy = 2 = screen without cursor readout,
c                             iy = 3 = plotter
c                             iy = 4 = file.
c*ip = 11 - 30  give back characters of names of devices
c               ix is device, iy contains character ip-10
c*ip = 10       give back number of defined devices in ix
c               give back capabillities in iy:
c               iy=  1 = can do clipping
c                   +2 = can do polyfill
c                   +4 = can do block fill
c                   +8 = can do pen clear
c                  +16 = can do pen invert
c*ip =  9       set pen width (ix) and height (iy)
c*ip = -8       unset clipping rectangle
c*ip =  8       set clipping rectangle from old position to ix,iy
c*ip =  7       end defining and draw filled polygon with color
c       ix=style, iy=pattern number
c       ix=0=hollow
c       ix=1=solid
c       ix=2=pattern
c       ix=3=pattern
c*ip =  6       start defining polygon, ix,iy is starting point
c*ip =  5       fill rectangle from old position to ix,iy with color
c*ip =  4       set pen to color ix (mod iy)
c               coding of color in big color pallette
c               ix = ired + iy*igreen + iy**2 * iblue
c*ip =  3       set pen to clear mode
c*ip =  2       set pen to invert mode
c*ip =  1       set pen to set mode
c*ip =  0       move pen to new coordinates (ix,iy)
c*ip =  -1      draw line to new coordinates (ix,iy)
c*ip =  -2      initialize for new plot (clear screen, new paper)
c       ix = 1 tektronics shape (1024*780)
c       ix = 2 three quarter size tektronics shape (768*585)
c       ix = 3 half size tektronics shape (512*390)
c       ix = 4 quarter size tektronics shape (256*195)
c       ix = 5 full screen
c       ix = 6 higz_cern
c*ip =  -3      activates cursor and performes readout (request mode)
c*ip =  -13     activates cursor and performes readout (sample mode)
c*ip =  -4      give back maximum possible ix and iy coordinates
c*ip =  -14     give back position of window on display in ix and iy
c*ip =  -5      give back number of pixels (steps) per unit of length
c*ip =  -6      give back supported number of colors (2-16777216) (8)
c*ip =  -999    stop plotting for the moment, it will continue later,
c               in the mean time terminal i/o might be undertaken.
c*ip =  -9999   bring graphics window to the front
c
c Function codes 10-31 are special and used to inform the software layer
c above about available devices. These calls (10-31) are also passed to the
c underlying layer but can be done also if there is no device open.
c-
!!SETC USINGINCLUDES = .FALSE.
!!T72-
        include 'types.f'
        include 'OSUtils.f'
        include 'Windows.f'
        include 'QDOffscreen.f'
        include 'Quickdraw.f'
        include 'globals.f'
c
        integer    minus1
        parameter (minus1 = -1)
        record /Cursor/         mycursor
        record /rect/           bounds,localrect,myportrect,updaterect
        record /WindowPeek/     front
        record /SysEnvRec/      myenv
        record /PolyHandle/     mypoly
        record /Point/          pnLoc,pnt1,pnt2
        record /CWindowPtr/     myCWindow
        record /GWorldptr/      myGWorld,tmpGWorld
        record /RGBcolor/       currcolor,colorblack,colorwhite
        record /GDHandle/       mywGDHandle,mygGDHandle,tmpGDHandle
        record /grafport/       myGport
        record /Pattern/        mypattern
        parameter (mxbf=500)
        logical jcdraw,jpoly
        logical jclgra,jfrmst,jmouse
        common /mpwevt/jclgra,jfrmst,jmouse,ixcur,iycur,
     #  myCWindow,myGWorld,
     #  mywGDHandle,mygGDHandle,colorblack,colorwhite,myGport,
     #  ixl(mxbf),iyl(mxbf),ipl(mxbf),updsec,mycursor,bounds,
     #  localrect,myportrect,updaterect,front,myenv,mypoly,pnLoc,
     #  currcolor,tmpGWorld,tmpGDHandle,jpoly,pnt1,pnt2
c
        logical jopn,jact,jfrm,jprev
        logical jtitle
        character chtit*(*)
        parameter (mxdv=8,mxnd=mxdv-1)
        character txt*80,cdev(mxdv)*20
        integer itdv(mxdv)
        integer ixpos(mxdv),iypos(mxdv),ixwid(mxdv),iywid(mxdv)
        string*255 title
        logical*1 visible,goAway
        integer*1 ipatt(8,9)
        save
        data ipatt/
     #  Z'aa',Z'55',Z'aa',Z'55',Z'aa',Z'55',Z'aa',Z'55',
     #  Z'44',Z'11',Z'44',Z'11',Z'44',Z'11',Z'44',Z'11',
     #  Z'00',Z'44',Z'00',Z'11',Z'00',Z'44',Z'00',Z'11',
     #  Z'80',Z'40',Z'20',Z'10',Z'08',Z'04',Z'02',Z'01',
     #  Z'20',Z'40',Z'80',Z'01',Z'02',Z'04',Z'08',Z'10',
     #  Z'44',Z'44',Z'44',Z'44',Z'44',Z'44',Z'44',Z'44',
     #  Z'00',Z'00',Z'00',Z'ff',Z'00',Z'00',Z'00',Z'ff',
     #  Z'11',Z'b8',Z'7c',Z'3a',Z'11',Z'a3',Z'c7',Z'8b',
     #  Z'10',Z'10',Z'28',Z'c7',Z'01',Z'01',Z'82',Z'7c'/
        data cdev/'s4_tek_screen','s3_tek_screen','s2_tek_screen',
     #  's1_tek_screen','mac_screen','macflat','machigh','higz_cern'/
        data ixpos/4,4,4,4,4,4,4,4/
        data iypos/40,40,40,40,40,40,40,40/
        data ixwid/1024,768,512,256,10000,580,290,620/
        data iywid/ 780,585,390,195,10000,290,580,436/
        data itdv/1,1,1,1,1,1,1,1/
        data jact/.false./,jopn/.false./,jtitle/.false./
        data nbfl/0/,ipenw/1/,ipenh/1/
c-----------------------------------------------------------------------
c first, check for the information function calls
c
        if(ip.ge.10.and.ip.le.31)then
         if(ip.eq.10)then
          ix=mxdv
          iy=1+2+4+8+16
         elseif(ip.eq.31)then
          iy=itdv(max(1,min(mxdv,ix)))
         else
          iy=ichar(cdev(max(1,min(mxdv,ix)))(ip-10:ip-10))
         endif
         return
        endif
c-----------------------------------------------------------------------
c in all cases except initialisation the workstation must be activated.
c
        if(.not.jopn.and.ip.ne.-2)return
        if(jopn.and..not.jact)then
         jact=.true.
         updsec=secnds(0.0)
        endif
c-----------------------------------------------------------------------
        if(ip.ge.-1.and.ip.le.9.or.ip.eq.-8)then
         nbfl=nbfl+1
         ixl(nbfl)=ix
         iyl(nbfl)=iy
         ipl(nbfl)=ip
         if(ip.eq.-1.or.ip.eq.0.or.ip.eq.5.or.ip.eq.8)then
          if(idev.le.mxnd)iyl(nbfl)=iyen-iyl(nbfl)
          ixl(nbfl)=ixl(nbfl)-ipenw/2
          iyl(nbfl)=iyl(nbfl)-ipenh/2
         elseif(ip.eq.9)then
          ipenw=max(ix,1)
          ipenh=max(iy,1)
         endif
         if(nbfl.lt.mxbf.and.abs(secnds(updsec)).le.1.0)return
        elseif(ip.eq.-4)then
         ix=ixen
         iy=iyen
         return
        elseif(ip.eq.-14)then
         ix=bounds.left
         iy=bounds.top
         return
        elseif(ip.eq.-5)then
         ix=min((ixen-ixbg+1)/10.24,(iyen-iybg+1)/7.80) + .001
         iy=ix
         return
        elseif(ip.eq.-6)then
         ix=mxcl
         iy=mxgr
         return
        endif
c-----------------------------------------------------------------------
c       in all cases except draw (modes) the buffer must be flushed
c
        if(jopn.and.nbfl.gt.0)then
         call GetGWorld(%ref(tmpGWorld.P),%ref(tmpGDHandle.H))
         call SetGWorld(myGWorld,mygGDHandle)
         ier=LockPixels(myGWorld.P^.portPixMap)
         do 10 i=1,nbfl
          if(ipl(i).eq.0)then
           call MoveTo(int2(ixl(i)),int2(iyl(i)))
           ixold=ixl(i)
           iyold=iyl(i)
           pnt2.h=ixl(i)
           pnt2.v=iyl(i)
          elseif(ipl(i).eq.-1)then
           call LineTo(int2(ixl(i)),int2(iyl(i)))
           ixold=ixl(i)
           iyold=iyl(i)
           pnt1=pnt2
           pnt2.h=ixl(i)
           pnt2.v=iyl(i)
           if(.not.jpoly)then
            call Pt2Rect(pnt1,pnt2,localrect)
            call unionrect(localrect,updaterect,updaterect)
           endif
          elseif(ipl(i).eq.9)then
           call PenSize(int2(ixl(i)),int2(iyl(i)))
          elseif(ipl(i).eq.-8)then
           call ClipRect(myGWorld.P^.portRect)
          elseif(ipl(i).eq.8)then
           if(idev.le.mxnd)then
            call setrect(localrect,int2(ixold),int2(iyl(i)),
     #                   int2(ixl(i)+1),int2(iyold+1))
           else
            call setrect(localrect,int2(ixold),int2(iyold),
     #                   int2(ixl(i)+1),int2(iyl(i)+1))
           endif
           call ClipRect(localrect)
          elseif(ipl(i).eq.7)then
           call ClosePoly
           if(ixl(i).eq.1)then
            call PaintPoly(mypoly)
           elseif(ixl(i).eq.2.or.ixl(i).eq.3)then
            np=max(1,min(9,iyl(i)))
            mypattern.pat(0)=ipatt(1,np)
            mypattern.pat(1)=ipatt(2,np)
            mypattern.pat(2)=ipatt(3,np)
            mypattern.pat(3)=ipatt(4,np)
            mypattern.pat(4)=ipatt(5,np)
            mypattern.pat(5)=ipatt(6,np)
            mypattern.pat(6)=ipatt(7,np)
            mypattern.pat(7)=ipatt(8,np)
            call FillPoly(mypoly,mypattern)
           else
            call FramePoly(mypoly)
           endif
           call unionrect(mypoly.H^.P^.polyBBox,updaterect,updaterect)
           call KillPoly(mypoly)
           jpoly=.false.
          elseif(ipl(i).eq.6)then
           mypoly.H=OpenPoly
           jpoly=.true.
          elseif(ipl(i).eq.5)then
           if(idev.le.mxnd)then
            call setrect(localrect,int2(ixold),int2(iyl(i)),
     #                   int2(ixl(i)+1),int2(iyold+1))
           else
            call setrect(localrect,int2(ixold),int2(iyold),
     #                   int2(ixl(i)+1),int2(iyl(i)+1))
           endif
           call paintrect(localrect)
           call unionrect(localrect,updaterect,updaterect)
          elseif(ipl(i).eq.4)then
           if(mxcl.gt.2.or.mxgr.gt.2)then
            ncl=max(2,iyl(i))
            fc=65535./float(ncl-1)
            ic=abs(ixl(i))
            currcolor.red  = nint(fc*mod(ic,ncl))
            ic=ic/ncl
            currcolor.green= nint(fc*mod(ic,ncl))
            ic=ic/ncl
            currcolor.blue = nint(fc*mod(ic,ncl))
            call RGBForeColor(currcolor)
           else
            if(ixl(i).eq.iyl(i)*iyl(i)*iyl(i)-1)then
             call RGBForeColor(colorwhite)
            else
             call RGBForeColor(colorblack)
            endif
           endif
          elseif(ipl(i).eq.3)then
           call PenMode(patBic)
          elseif(ipl(i).eq.2)then
           call PenMode(patXor)
          elseif(ipl(i).eq.1)then
           call PenMode(patCopy)
          endif
10       continue
         call UnlockPixels(myGWorld.P^.portPixMap)
         call SetGWorld(tmpGWorld,tmpGDHandle)
         nbfl=0
         if((ip.ge.-1.and.ip.le.9.or.ip.eq.-8).and.
     #   abs(secnds(updsec)).le.1.0)return
        endif
c-----------------------------------------------------------------------
        if(ip.eq.999)then
         call DisposeGWorld(myGWorld)
         myGWorld.P=nil
         call DisposeWindow(myCWindow.P)
         myCWindow.P=nil
         jopn=.false.
         jact=.false.
c-----------------------------------------------------------------------
        elseif(ip.eq.-2)then
         if(.not.jopn)then
          front.P=FrontWindow()
          if(front.P.eq.nil)write(6,*)' '
!!IFC NOT LSPOWERF
          ierror = SysEnvirons(curSysEnvVers,myenv)
          jcdraw = myenv.hasColorQD
          if(.not.jcdraw)then
           write(6,*)'macplo: Color Quickdraw is not installed!'
           write(6,*)'macplo: update your system.'
           return
          endif
          QDG = JQDGLOBALS()
!!ELSEC
          QDG = %loc(qd)
!!ENDC
          idev=max(1,min(mxdv,ix))
          isl=QDG^.screenbits.bounds.left   +  4
          isr=QDG^.screenbits.bounds.right  -  4
          ist=QDG^.screenbits.bounds.top    + 40
          isb=QDG^.screenbits.bounds.bottom -  4
c
          if(idev.ge.1.and.idev.le.mxnd)then
           call fclenv('plot_windowposition',txt)
           call plcwps(isl,isr,ist,isb,ixwid(idev),iywid(idev),
     #     txt,iwl,iwr,iwt,iwb)
           if(idev.le.4)then
            f=min(float(iwb - iwt + 1)/780.,float(iwr - iwl + 1)/1024.)
            iwb = iwt  + nint(f *  780.) - 1
            iwr = iwl  + nint(f * 1024.) - 1
           endif
          else
           iwr = min(isr,max(isl,ixpos(idev))+ixwid(idev))
           iwl = max(isl,iwr-ixwid(idev))
           iwb = min(isb,max(ist,iypos(idev))+iywid(idev))
           iwt = max(ist,iwb-iywid(idev))
          endif
c
          if(mod(iwr-iwl+1,2).eq.1)iwr=iwr+1
          if(mod(iwb-iwt+1,2).eq.1)iwb=iwb+1
          ixbg=0
          iybg=0
          ixen=iwr-iwl
          iyen=iwb-iwt
          bounds.right  = iwr
          bounds.left   = iwl
          bounds.bottom = iwb
          bounds.top    = iwt
c
          if(.not.jtitle)then
           write(txt,'(a,i5,a,i5,a)')
     #     'MacPlo =>',ixen+1,'*',iyen+1,' pixels'
           title   = txt(1:27)
          endif
          visible = .true.
          goAway  = .true.
          myCwindow.P=nil
          mywGDHandle.H=nil
          myGWorld.P=nil
          myCWindow.P = NewCWindow(nil,bounds,title,visible,
     #                  int2(noGrowDocProc),minus1,goAway,nil)
          call SetPort(myCWindow.P)
          myportrect=myCWindow.P^.portRect
          call ClipRect(myCWindow.P^.portRect)
          call PenMode(patCopy)
          colorblack.red   = 0
          colorblack.green = 0
          colorblack.blue  = 0
          colorwhite.red   = -1
          colorwhite.green = -1
          colorwhite.blue  = -1
          call RGBForeColor(colorblack)
          call RGBBackColor(colorwhite)
          call GetGWorld(%ref(myCWindow.P),%ref(mywGDHandle.H))
          mxcl=myCWindow.P^.portpixmap.H^.P^.pixelsize
          if(mxcl.eq.16)mxcl=15
          mxcl=2**max(1,min(24,mxcl))
          mxgr=min(256,mxcl)
          ier=NewGWorld(%ref(myGWorld.P),int2(0),bounds,nil,nil,nil)
          if(ier.ne.0)then
           call DisposeWindow(myCWindow.P)
           myCWindow.P=nil
           write(6,*)'macplo: GWorld creation failed, error:',ier
           write(6,*)'macplo: allocate more memory to this application'
           write(6,*)'bounds.right,bounds.left,bounds.bottom,bounds.top'
           write(6,*)bounds.right,bounds.left,bounds.bottom,bounds.top
           return
          endif
          mygGDHandle.H=nil
          jopn=.true.
          jact=.true.
         elseif(jtitle)then
          call SetWTitle(myCWindow.P,title)
         endif
c
         call SetGWorld(myGWorld,mygGDHandle)
         ier=LockPixels(myGWorld.P^.portPixMap)
         call RGBForeColor(colorblack)
         call RGBBackColor(colorwhite)
         call ClipRect(myGWorld.P^.portRect)
         call EraseRect(myGWorld.P^.portRect)
         call UnlockPixels(myGWorld.P^.portPixMap)
         nbfl=0
         jpoly=.false.
         updaterect=myportrect
c-----------------------------------------------------------------------
        elseif(ip.eq.-3.or.ip.eq.-13)then
c
         call SetGWorld(myCWindow,mywGDHandle)
         do 90 i=0,15
          mycursor.mask.bits(i)=$0000
90       continue
         mycursor.data=mycursor.mask
         mycursor.hotSpot.h=8
         mycursor.hotSpot.v=8
         ixcur=-11111
         iycur=-11111
         jfrm=.true.
c
100      jprev=jfrm
         jclgra=.false.
         jmouse=.false.
         call F_DoBackground
         jfrm=jfrmst
         if(.not.jprev.and.jfrm)then
110       if(button)goto 110
          goto 100
         endif
c
         if(jclgra)then
          ixx=0
          iyy=iyen
          goto 190
         endif
c
         if(myCWindow.P.ne.FrontWindow())goto 100
c
c       draw (parts of) new cursor and erase (parts of) old cursor
c
         call SetGWorld(myCWindow,mywGDHandle)
         if(.not.jfrm)then
          ixx=-11111
          iyy=-11111
         else
          call GetMouse(%ref(pnLoc))
          ixx=pnLoc.h
          iyy=pnLoc.v
         endif
         if(ixx.lt.0.or.ixx.gt.ixen.or.iyy.lt.0.or.iyy.gt.iyen)then
          ixx=-11111
          iyy=-11111
         endif
         if(ixx.ne.ixcur.or.iyy.ne.iycur)then
          call SetCursor(mycursor)
          if(ip.eq.-3)then
           call PenMode(patXor)
           if(ixx.ne.ixcur)then
            call MoveTo(int2(ixx),int2(iybg))
            call LineTo(int2(ixx),int2(iyen))
            call MoveTo(int2(ixcur),int2(iybg))
            call LineTo(int2(ixcur),int2(iyen))
            ixcur=ixx
           endif
           if(iyy.ne.iycur)then
            call MoveTo(int2(ixbg),int2(iyy))
            call LineTo(int2(ixen),int2(iyy))
            call MoveTo(int2(ixbg),int2(iycur))
            call LineTo(int2(ixen),int2(iycur))
            iycur=iyy
           endif
           call PenMode(patCopy)
          endif
         endif
c
         if(ixx.ge.0.and.ixx.le.ixen.and.iyy.ge.0.and.iyy.le.iyen)then
          if(jmouse.or.ip.eq.-13)then
130        if(button)goto 130
           goto 190
          endif
         else
          call InitCursor
         endif
         goto 100
c
190      call PenMode(patXor)
         call MoveTo(int2(ixcur),int2(iybg))
         call LineTo(int2(ixcur),int2(iyen))
         call MoveTo(int2(ixbg),int2(iycur))
         call LineTo(int2(ixen),int2(iycur))
         call PenMode(patCopy)
         call InitCursor
         ix=max(0,min(ixen,ixx))
         iy=max(0,min(iyen,iyy))
         if(idev.le.mxnd)iy=iyen-iy
c-----------------------------------------------------------------------
        elseif(ip.eq.-999)then
         jact=.false.
c-----------------------------------------------------------------------
        elseif(ip.eq.-9999)then
         front.P=FrontWindow()
         if(front.P^.windowKind.ne.dBoxProc)then
ccc       call GetGWorld(%ref(tmpGWorld.P),%ref(tmpGDHandle.H))
          call SetGWorld(myCWindow,mywGDHandle)
          call SelectWindow(myCWindow)
ccc       call SetGWorld(tmpGWorld,tmpGDHandle)
         endif
        endif
c-----------------------------------------------------------------------
        if(jopn)then
         if(.not.EmptyRect(updaterect))then
ccc       call GetGWorld(%ref(tmpGWorld.P),%ref(tmpGDHandle.H))
          call SetGWorld(myCWindow,mywGDHandle)
          updaterect.left  =updaterect.left  -ipenw/2
          updaterect.top   =updaterect.top   -ipenh/2
          updaterect.right =updaterect.right +ipenw
          updaterect.bottom=updaterect.bottom+ipenh
          call InvalRect(updaterect)
          call SetRect(updaterect,int2( 10000),int2( 10000),
     #                            int2(-10000),int2(-10000))
ccc       call SetGWorld(tmpGWorld,tmpGDHandle)
         endif
         call F_DoBackground
         updsec=secnds(0.0)
        endif
        return
c-----------------------------------------------------------------------
        entry imsettit(lentit,chtit)
        if(lentit.gt.0.and.lentit.le.255)then
         title=chtit(1:lentit)
         jtitle=.true.
        else
         jtitle=.false.
        endif
        return
c-----------------------------------------------------------------------
        entry imsetwsz(ixp,iyp,ixw,iyh)
        ixpos(5)=ixp
        iypos(5)=iyp
        ixwid(5)=ixw
        iywid(5)=iyh
        ixpos(6)=ixp
        iypos(6)=iyp
        ixwid(6)=ixw
        iywid(6)=iyh
        return
c-----------------------------------------------------------------------
        entry imgetds(idxp,idyp,idxw,idyh)
!!IFC NOT LSPOWERF
        QDG = JQDGLOBALS()
!!ELSEC
        QDG = %loc(qd)
!!ENDC
        idxp=QDG^.screenbits.bounds.left
        idyp=QDG^.screenbits.bounds.top
        idxw=QDG^.screenbits.bounds.right  - QDG^.screenbits.bounds.left
        idyw=QDG^.screenbits.bounds.bottom - QDG^.screenbits.bounds.top
        return
c-----------------------------------------------------------------------
        entry imgetms(im)
        if(jmouse)then
         im=2
        else
         im=0
        endif
        end
#endif
