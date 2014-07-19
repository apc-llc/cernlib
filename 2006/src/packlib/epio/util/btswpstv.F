*
* $Id: btswpstv.F,v 1.1.1.1 1996/03/08 15:21:48 mclareni Exp $
*
* $Log: btswpstv.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:48  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (!defined(CERNLIB_STF77))&&(defined(CERNLIB_STF77VX))
      SUBROUTINE BTSWAP(SOURCE,NBYTES)
CLatB  Aleph version
      implicit none
      integer*2 source(*)
      integer nbytes

      integer*2 word0
      integer*1 byte0(2)
      equivalence(byte0,word0)
      integer*2 word1
      integer*1 byte1(2)
      equivalence(byte1,word1)
      integer i

      if (nbytes.eq.0) return
      do i=1,nbytes/2
         word0=source(i)
         byte1(1)=byte0(2)
         byte1(2)=byte0(1)
         source(i)=word1
      end do

      return
      end
#endif
