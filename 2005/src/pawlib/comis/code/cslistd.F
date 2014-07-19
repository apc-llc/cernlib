*
* $Id: cslistd.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: cslistd.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 28/09/94  18.26.08  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   14/12/93
      SUBROUTINE CSLISTD(PROADDR)
***--------------------------------------------------------------
      CHARACTER LASTNM*32
      INTEGER PROADDR
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cscbwl.inc"
      I=ICBWL
 2    IF(I.EQ.0)RETURN
      LASTNM=' '
      NCHS=IQ(I+3)
      CALL UHTOC(IQ(I+4),4,LASTNM,NCHS)
      IOFFS=-2
      CALL CSLISTX(LASTNM,PROADDR,IOFFS,NEL)
      I=IQ(I)
      GO TO 2
      END
