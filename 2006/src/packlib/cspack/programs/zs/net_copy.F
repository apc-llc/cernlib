*
* $Id: net_copy.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: net_copy.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      SUBROUTINE Net_Copy(iad1,nw,iword,istat)
*.==========>
*.           To copy NW words starting at IQ(iad1)
*.           from (CPU,Module) to server address IWORD(1)                t
*..=========> ( R.Brun)
#include "cspack/hos9c.inc"
      dimension iword(1)
*
      if(nw.le.0)return
      loc1=iadpawd-4+4*(iad1+18)
      nbytes=4*nw
      len=net_getarr(cpu,loc1,nbytes,iword)
      if(len.ne.nbytes)then
         istat=1
      else
         istat=0
      endif
      END
#endif
