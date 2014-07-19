*
* $Id: imsetco.F,v 1.1.1.1 1996/02/14 13:11:00 mclareni Exp $
*
* $Log: imsetco.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:00  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.16/05 12/10/92  14.06.41  by  O.Couet
*-- Author : Cees de Laat
      subroutine imsetco(index,r,g,b)
c+
c       subroutine imsetco(index,r,g,b)
c Set color r,g,b for index. The requested color will be mapped to the
c closest available color in the map of the screen.
c
c Color index management, index between 0 and 255 inclusive.
c-
      parameter (mxcl=1000)
      integer irgb(0:255),indold
      save
      data irgb/256*0/,indold/-1/
c
      indold=-1
      if(index.ge.0.and.index.le.255)then
         if(r.ge.0..and.r.le.1..and.
     +      g.ge.0..and.g.le.1..and.
     +      b.ge.0..and.b.le.1.)then
            ir=nint(r*(mxcl-1))
            ig=nint(g*(mxcl-1))
            ib=nint(b*(mxcl-1))
            irgb(index)=ir+(ig+ib*mxcl)*mxcl
         endif
      endif
      return
c-----------------------------------------------------------------------
      entry imgetcol(index,r,g,b)
c+
c returns the requested color mapping of the index, not the on screen
c actual!!!
c+
      if(index.ge.0.and.index.le.255)then
         fc=1./float(mxcl-1)
         ic=irgb(index)
         r=nint(fc*mod(ic,mxcl))
         ic=ic/mxcl
         g=nint(fc*mod(ic,mxcl))
         ic=ic/mxcl
         b=nint(fc*mod(ic,mxcl))
      endif
      return
c-----------------------------------------------------------------------
      entry imsetcc(index)
c+
c       entry imsetcc(index)
c sets the color for all following drawing operations.
c=
      if(index.ge.0.and.index.le.255.and.index.ne.indold)then
         indold=index
         call imacplo(irgb(indold),mxcl,4)
      endif
      end
#endif
