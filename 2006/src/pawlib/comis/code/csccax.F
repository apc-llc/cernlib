*
* $Id: csccax.F,v 1.1.1.1 1996/02/26 17:16:27 mclareni Exp $
*
* $Log: csccax.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:27  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 28/09/93  16.39.14  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION CSCCAX(STR,NOPAR,M)
      INTEGER CSLTGP
      COMPLEX CSCCAX
      COMPLEX CSCJCX
      CHARACTER *(*)STR
      INTEGER M(10)
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#include "comis/csfres.inc"
#if !defined(CERNLIB_APOLLO)
      N=LEN(STR)
#endif
#if defined(CERNLIB_APOLLO)
      N=KLENID
#endif
      J=MJSCHA(STR)
      K=MKBLAN(J,N)
      IF(MIDENT(J,N,JID,KLENID).EQ.0)GO TO 3
      JP=CSLTGP(IPVS)
      GO TO 1
      ENTRY CSCJCX(JSUB,NOPAR,M)
      JP=JSUB
      CSCJCX=(0.,0.)
 1    CSCCAX=(0.,0.)
      IF(JP.LE.0)GO TO 4
      IF(NOPAR.LT.0)GO TO 3
 2    CALL CSJCAX(JP,NOPAR,M)
      CSCCAX=CXSRES
      RETURN
 3    REC1='CSCCAL: ERROR IN PARAMETER DESCRIPTION'
      GO TO 5
 4    REC1='CSCCAL: NO PROGRAM'
      CALL CCOPYS(JID,JSR1+20,NCIDEN)
 5    CALL CSSOUT(REC1)
      END
