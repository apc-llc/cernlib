*
* $Id: cscall.F,v 1.1.1.1 1996/02/26 17:16:26 mclareni Exp $
*
* $Log: cscall.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:26  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 28/09/93  16.30.30  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSCALL(STR,NPAR,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
***------------------------------------------------------------
*  a set of routine for calls comis-routine from f77-level
*  see user quide
***------------------------------------------------------------
      INTEGER CSLTGP
      CHARACTER *(*)STR
      INTEGER M(10),MD(11)
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#if defined(CERNLIB_APOLLO)
      N=KLENID
#endif
#if !defined(CERNLIB_APOLLO)
      N=LEN(STR)
#endif
      J=MJSCHA(STR)
      K=MKBLAN(J,N)
      IF(MIDENT(J,N,JID,KLENID).EQ.0)GO TO 3
      JP=CSLTGP(IPVS)
      GO TO 1
      ENTRY CSJCAL(JSUB,NPAR,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      JP=JSUB
  1   IF(JP.LE.0)GO TO 4
      IF(NPAR)3,2,10
 10   IF(NPAR.GT.10)GO TO 6
      DO 77 I=2,NPAR+1
 77   MD(I)=-101
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
 6    CALL CSSOUT('CSCALL: TOO MANY PARAMETERS')
      RETURN
 2    CALL CSJCAX(JP,NPAR,M)
      RETURN
 3    REC1='CSCALL: ERROR IN PARAMETER DESCRIPTION'
      GO TO 5
 4    REC1='CSCALL: NO PROGRAM'
      CALL CCOPYS(JID,JSR1+20,NCIDEN)
 5    CALL CSSOUT(REC1)
      END
