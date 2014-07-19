*
* $Id: dzdra1.F,v 1.1.1.1 1996/03/04 16:13:08 mclareni Exp $
*
* $Log: dzdra1.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:08  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDRA1(IXSTOR,LTOP, ILKACT
     & ,CHOPT, XCUR, YCUR, REQINP,IFLCUR,YTOP,YBOTTM)
*.
*...DZDRA1   interface routine DZDRAW => DZDRA2
*.           and               DZDGET => DZDRA2
*.           to hide ZEBRA internals from DZDRA2
*.
*. INPUT     : IXSTOR    Store index
*.             LTOP      Link to top bank to be displayed
*.             ILKACT    the link where drawing of down banks starts
*.             CHOPT     Character option (not yet used)
*.             REQINP    indicates if picture should be drawn by
*.                       DZDRA2 or locator input is requested
*. OUTPUT    : XCUR,YCUR coordinates of locator
*.             IFLCUR    flag indicating where locator is
*. COMMON    :
*. SEQUENCE  : MZCA   MZCB   QSTORE ZEBQ   ZUNIT
*. CALLS     : DZDRA2
*. CALLED    : DZDRAW, DZDGET
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
      CHARACTER*(*) CHOPT
      LOGICAL REQINP
*
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/zunit.inc"
#include "zebra/qstore.inc"
      CALL DZDRA2(IXSTOR,LQ(KQS+1), IQ(KQS+1),LTOP,ILKACT
     & ,CHOPT, XCUR, YCUR, REQINP, IFLCUR,YTOP,YBOTTM)
      RETURN
      END
************************************************************************
