*
* $Id: imopnwi.F,v 1.1.1.1 1996/02/14 13:11:00 mclareni Exp $
*
* $Log: imopnwi.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:00  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.23/06 01/11/95  14.04.54  by  O.Couet
*-- Author : Cees de Laat
      integer function imopnwi(ix,iy,iw,ih,lentit,chtit)
c+
c       integer function imopnwi(ix,iy,iw,ih,lentit,chtit)
c
c Opens a window on the screen at position ix,iy. Size of the window
c will be iw * ih pixels. Coordinates on display and in the window are
c increasing from top to down and from left to right.
c
c ix,iy,iw,iy <= integer, position and size of window
c lentit      <= integer, length of the window title. If a zero or negative
c                number is supplied, a default title will be generated.
c chtit       <= character string, title for the window.
c imopnwi     => integer, return status, -1 is failure.
c It is possible to open only one grapics window.
c-
      character chtit*(*)
      call imsettit(lentit,chtit)
      call imsetwsz(ix,iy,iw,ih)
      call imacplo(ima,idm,10)
      call imacplo(ima,0,-2)
      i=0
      j=0
      call imacplo(i,j,-4)
      if(i.eq.0.or.j.eq.0)then
         imopnwi=-1
      else
         imopnwi=0
         call imsetco(0,1.,1.,1.)
         call imsetco(1,0.,0.,0.)
      endif
      end
#endif
