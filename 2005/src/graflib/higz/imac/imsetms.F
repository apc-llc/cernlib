*
* $Id: imsetms.F,v 1.1.1.1 1996/02/14 13:11:01 mclareni Exp $
*
* $Log: imsetms.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:01  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.16/01 18/08/92  10.10.58  by  O.Couet
*-- Author : Cees de Laat
      subroutine imsetms(itype,n,ixy)
c+
c       subroutine imsetms(itype,n,ixy)
c
c Set marker type. The definition is the same as for the corresponding
c ixsetms routine, but the filled polygon routines are not implemented and
c will only produce the hollow shapes.
c
c itype    <= integer, marker type
c n        <= integer, number of points in ixy
c ixy(2,n) <= integer, array of x,y points in ixy(2,n)
c       n.eq.0 Marker is a single point
c       itype = 0 or 1, Marker is hollow circle of diameter N
c       itype = 2 or 3, Marker is hollow polygon described by ixy
c       itype = 4, Marker is described by segmented line ixy
c-
      integer ixy(2,*),mkxy(2,40)
      save
      data mktp/-1/,mknn/0/,nmkc/1/
c
      if(n.le.0.or.itype.lt.0.or.itype.gt.4)then
         mktp=-1
         mknn=0
      elseif(itype.eq.0.or.itype.eq.1)then
         mktp=itype
         fac=2.*3.14159264358/32.
         nr=max(1,n/2)
         mkxy(1,1)=nr
         mkxy(2,1)=0
         mknn=2
         do 10  i=2,32
            phi=float(i-1)*fac
            mkxy(1,mknn)=nint(nr*cos(phi))
            mkxy(2,mknn)=nint(nr*sin(phi))
            if(mkxy(1,mknn-1).ne.mkxy(1,mknn).or. mkxy(1,mknn-1)
     +      .ne.mkxy(1,mknn))mknn=mknn+1
   10    continue
         mkxy(1,mknn)=mkxy(1,1)
         mkxy(2,mknn)=mkxy(2,1)
      elseif(itype.eq.2.or.itype.eq.3.or.itype.eq.4)then
         mktp=itype
         mknn=min(n,40)
         do 20  i=1,mknn
            mkxy(1,i)=ixy(1,i)
            mkxy(2,i)=ixy(2,i)
   20    continue
      endif
      return
c-----------------------------------------------------------------------
      entry imsetmc(index)
c+
c       entry imsetmc(index)
c
c Set color index for markers
c index <= integer, index of color defined previously by imsetco
c-
      nmkc=index
      return
c-----------------------------------------------------------------------
      entry immarke(n,ixy)
c+
c       entry immarke(n,ixy)
c Sets a marker at each of the points in ixy
c
c ixy   <= integer, array of points where marker will be drawn.
c-
      call imsetcc(nmkc)
      do 40 i=1,n
         ixo=ixy(1,i)
         iyo=ixy(2,i)
         if(mktp.eq.-1)then
            call imacplo(ixo,iyo,0)
            call imacplo(ixo,iyo,-1)
         elseif(mktp.ge.0.and.mktp.le.4)then
            ip=0
            do 30 j=1,mknn
               ix=ixy(1,i)+mkxy(1,j)
               iy=ixy(2,i)+mkxy(2,j)
               call imacplo(ix,iy,ip)
               if(mktp.eq.4)then
                  ip=-1-ip
               else
                  ip=-1
               endif
   30       continue
         endif
   40 continue
      end
#endif
