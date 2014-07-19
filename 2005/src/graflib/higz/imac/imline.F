*
* $Id: imline.F,v 1.1.1.1 1996/02/14 13:11:00 mclareni Exp $
*
* $Log: imline.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:00  mclareni
* Higz
*
*
#if defined(CERNLIB_MACMPW)
#include "higz/pilot.h"
*CMZ :  1.16/04 02/10/92  13.37.17  by  O.Couet
*-- Author : Cees de Laat
      subroutine imline(n,ixy)
c+
c       subroutine imline(n,ixy)
c
c Draw a line trough the points given in ixy(2,n)
c n     <= integer, number of points
c ixy   <= integer array, set of points to draw the line through.
c-
      integer ixy(2,*)
      data indxl/1/
      save
c
      call imsetcc(indxl)
      ip=0
      do 10 i=1,n
         call imacplo(ixy(1,i),ixy(2,i),ip)
         ip=-1
   10 continue
      return
c-----------------------------------------------------------------------
      entry imsetlc(index)
      indxl=index
      return
c-----------------------------------------------------------------------
      end
#endif
