*
* $Id: csdcax.F,v 1.1.1.1 1996/02/26 17:16:27 mclareni Exp $
*
* $Log: csdcax.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:27  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 28/09/93  16.38.36  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION CSDCAX(STR,NOPAR,M)
      INTEGER CSLTGP
      DOUBLE PRECISION CSDCAX
      DOUBLE PRECISION CSDJCX
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
      ENTRY CSDJCX(JSUB,NOPAR,M)
      JP=JSUB
      CSDJCX=0.D0
 1    CSDCAX=0.D0
      IF(JP.LE.0)GO TO 4
      IF(NOPAR.LT.0)GO TO 3
 2    CALL CSJCAX(JP,NOPAR,M)
      CSDCAX=DCSRES
      RETURN
 3    REC1='CSDCAL: ERROR IN PARAMETER DESCRIPTION'
      GO TO 5
 4    REC1='CSDCAL: NO PROGRAM'
      CALL CCOPYS(JID,JSR1+20,NCIDEN)
 5    CALL CSSOUT(REC1)
      END
