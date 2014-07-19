*
* $Id: ifaf3.F,v 1.1 1999/10/20 14:12:57 couet Exp $
*
* $Log: ifaf3.F,v $
* Revision 1.1  1999/10/20 14:12:57  couet
* - ifaf3.F needed in ig3
*
* Higz
*
*
#include "higz/pilot.h"
      subroutine ifaf3(n,x,y,z,f)
************************************************************************
*
************************************************************************
      real x(*),y(*),z(*),f(*),p(3)
      real xx(30),yy(30),zz(30)
*-
      do i=1,n
        p(1) = x(i)
        p(2) = y(i)
        p(3) = z(i)
        call ihwton(p,p)
        xx(i) = p(1)
        yy(i) = p(2)
        zz(i) = p(3)
      end do

*      znorm = (xx(3)-xx(1))*(yy(4)-yy(2)) - (xx(4)-xx(2))*(yy(3)-yy(1))
*      if (znorm .le. 0) return

      call ihzsha(n,xx,yy,zz,f)
      end

