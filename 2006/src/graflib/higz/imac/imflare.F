*
* $Id: imflare.F,v 1.1.1.1 1996/02/14 13:11:01 mclareni Exp $
*
* $Log: imflare.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:01  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.19/03 24/08/93  11.37.18  by  O.Couet
*-- Author :    Cees de Laat
        subroutine imflare(n,ixy)
c+
c       subroutine imflare(n,ixy)
c
c Draw a line trough the points given in ixy(2,n)
c n     <= integer, number of points
c ixy   <= integer array, set of points to draw the line through.
c
c style = 0 = hollow
c         1 = solid
c         2 = pattern
c         3 = hatch
c-
      integer ixy(2,*),ixyloc(2,5)
      data indxf/1/,istyl/0/,ifasi/2/
      save indxf,istyl,ifasi
c
      call imsetcc(indxf)
      call imacplo(0,0,6)
      ip=0
      do 10 i=1,n
          call imacplo(ixy(1,i),ixy(2,i),ip)
        ip=-1
   10 continue
      call imacplo(istyl,ifasi,7)
      return
c-----------------------------------------------------------------------
      entry imsetfc(index)
      indxf=index
      return
c-----------------------------------------------------------------------
      entry imsetfs(istyle,ifasii)
      istyl=max(0,min(3,istyle))
      ifasi=ifasii
      if(ifasi.le.0.or.ifasi.gt.9)ifasi=2
      return
c-----------------------------------------------------------------------
      entry imbox(ix1,ix2,iy1,iy2,mode)
c+
c       subroutine imbox(ix1,ix2,iy1,iy2,mode)
c Draws a rectangle
c mode not equals 0 rect = filled with current color.
c-
      if(mode.eq.0)then
        ixyloc(1,1)=ix1
        ixyloc(2,1)=iy1
        ixyloc(1,2)=ix2
        ixyloc(2,2)=iy1
        ixyloc(1,3)=ix2
        ixyloc(2,3)=iy2
        ixyloc(1,4)=ix1
        ixyloc(2,4)=iy2
        ixyloc(1,5)=ix1
        ixyloc(2,5)=iy1
        call imline(5,ixyloc)
      else
        call imsetcc(indxf)
        call imacplo(min(ix1,ix2),min(iy1,iy2),0)
        call imacplo(max(ix1,ix2),max(iy1,iy2),5)
      endif
      end
#endif
