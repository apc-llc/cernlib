*
* $Id: dzdsyw.F,v 1.1.1.1 1996/03/04 16:13:10 mclareni Exp $
*
* $Log: dzdsyw.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:10  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSYW(IXSTOR, L, LUNPRT)
*.
*...DZDSYW  dump system words and links, interface routine to
*.          hide ZEBRA internals from DZDSY1
*.
*. INPUT     : IXSTOR   store index
*.             L        link to bank to be dumped
*.             LUNPRT   unit for printing
*.
*. COMMON    :
*. SEQUENCE  : MZCA   MZCB   QSTORE ZEBQ   ZUNIT
*. CALLS     : DZDSY1
*. CALLED    : DZDISP
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
*
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/zunit.inc"
#include "zebra/qstore.inc"
      CALL DZDSY1(LQ(KQS+1),IQ(KQS+1),L, LUNPRT)
      RETURN
      END
************************************************************************
