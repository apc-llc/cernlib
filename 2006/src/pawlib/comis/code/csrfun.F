*
* $Id: csrfun.F,v 1.1.1.1 1996/02/26 17:16:26 mclareni Exp $
*
* $Log: csrfun.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:26  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 28/09/93  16.33.31  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION CSRFUN(STR,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      INTEGER CSLTGP,CSIPPD
      CHARACTER *(*)STR
      INTEGER M(10),MD(11)
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#include "comis/csfres.inc"
#if !defined(CERNLIB_APOLLO)
      N=LEN(STR)
#endif
#if defined(CERNLIB_APOLLO)
      N=80
#endif
      J=MJSCHA(STR)
      K=MKBLAN(J,N)
      IF(MIDENT(J,N,JID,KLENID).EQ.0)GO TO 3
      JP=CSLTGP(IPVS)
      GO TO 1
      ENTRY CSRJFN(JSUB,STR,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      J=MJSCHA(STR)
      N=80
      JP=JSUB
      CSRJFN=0.
 1    CSRFUN=0.
      IF(JP.LE.0)GO TO 4
      NPAR=CSIPPD(J,N,MD)
      IF(NPAR)3,2,10
 10   IF(NPAR.GT.10)GO TO 6
      GO TO (11,12,13,14,15,16,17,18,19,20),NPAR
 11      CALL CSPMD1(M,MD,P1)
      GO TO 2
 12      CALL CSPMD2(M,MD,P1,P2)
      GO TO 2
 13      CALL CSPMD3(M,MD,P1,P2,P3)
      GO TO 2
 14      CALL CSPMD4(M,MD,P1,P2,P3,P4)
      GO TO 2
 15      CALL CSPMD5(M,MD,P1,P2,P3,P4,P5)
      GO TO 2
 16      CALL CSPMD6(M,MD,P1,P2,P3,P4,P5,P6)
      GO TO 2
 17      CALL CSPMD7(M,MD,P1,P2,P3,P4,P5,P6,P7)
      GO TO 2
 18      CALL CSPMD8(M,MD,P1,P2,P3,P4,P5,P6,P7,P8)
      GO TO 2
 19      CALL CSPMD9(M,MD,P1,P2,P3,P4,P5,P6,P7,P8,P9)
      GO TO 2
 20      CALL CSPMD0(M,MD,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      GO TO 2
 6    CALL CSSOUT('CSRFUN: TOO MANY PARAMETERS')
      RETURN
 2    CALL CSPCLL(JP,MD,M)
      CSRFUN=RCSRES
      RETURN
 3    REC1='CSRFUN: ERROR IN PARAMETER DESCRIPTION'
      GO TO 5
 4    REC1='CSRFUN: NO PROGRAM'
      CALL CCOPYS(JID,JSR1+20,NCIDEN)
 5    CALL CSSOUT(REC1)
      END
