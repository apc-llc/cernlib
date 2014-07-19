*
* $Id: nettab.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: nettab.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      SUBROUTINE Net_HTAB(chcpu,chmod,istat)
*.==========>
*.           To copy the histogram address table from
*.           (CPU,Module) to server memory
*.           IADPAWD is the absolute address of /PAWC/ in node
*.                  =net_mad(cpu,module)
*..=========> ( R.Brun)
*
#include "cspack/hos9c.inc"
      CHARACTER*20 chcpu,chmod
      parameter (knrh=6)
      parameter (klq =-8)
*.___________________________________________
*
*   lcdir=lq(1)       = iq(klq+1)
*   ltab =lq(lcdir-3) = iq(klq+lcdir-3)
*
      istat=0
      cpu=chcpu
      module=chmod
      iadpawd=net_mad(cpu,module)
      if(iadpawd.eq.-1)go to 90
*
*           Copy address table,list of IDs and headers
*
      nhist= Net_Hlink(cpu,module,ihead)
      if(nhist.ne.-1)go to 99
*
  90  nhist=-1
      istat=1
*
  99  RETURN
      END
#endif
