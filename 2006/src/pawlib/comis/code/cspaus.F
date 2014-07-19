*
* $Id: cspaus.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: cspaus.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 16/02/94  15.43.48  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPAUS(PR)
***----------------------------
*      the entry to comis.
*
***----------------------------
       INTEGER CSPMFS,CSINT1
         CHARACTER *(*) PR
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/cslun.inc"
#include "comis/csichv.inc"
         COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
         CHARACTER*8 PRT(2),PRMAIN(2),QUIT*4,EXIT*4
         DATA MNRECU/0/,PRMAIN/'*MAIN1','*MAIN2'/
         DATA QUIT/'QUIT'/, EXIT/'EXIT'/
*----------------------
      MNRECU=MNRECU+1
      JQUIT=MJSCHA(QUIT)
      JEXIT=MJSCHA(EXIT)
      IF(MNRECU.GT.2)THEN
        CALL MDPAUS('  RECURSION IN CSPAUS>2 !!!!')
        MNRECU=2
      ENDIF
      PRT(MNRECU)=PR
      CALL CCOPYS(MJSCHA(PRMAIN(MNRECU)),JSMAIN,8)
      IF(MNRECU.EQ.2)THEN
        ISVFIL=ISTFIL
        ISTFIL=0
      ENDIF
  1   IF(ISTFIL.EQ.0.AND.LUNINP.EQ.5) CALL CSSOUT(PRT(MNRECU))
  2   ISTATE=1
      CALL CSRD(JGSST,NGSST)
      IF (NGSST .LT. 0) GO TO 4
      K=MKBLAN(JGSST,NGSST)
      IF(NGSST.EQ.0)GO TO 2
      IF(K.EQ.ICHSX)GO TO 4
      IF(K.EQ.ICHSE)THEN
*   !DIRECTIVE
        JGSST=JGSST+1
        NGSST=NGSST-1
        IF(KEYRD.NE.-1)THEN
          KEYRD=0
        ENDIF
        CALL CSPDIR(IERR)
        IF(KEYRD.NE.-1 .AND. IERR.EQ.0)CALL CSTLOG(JPMB)
        GO TO 2
      ENDIF
      IF(NGSST.LE.4)THEN
        IF(NGSST.EQ.4)THEN
*QUIT? or EXIT
          IF(MLEQS(JQUIT,JGSST,4).EQ.1)GO TO 4
          IF(MLEQS(JEXIT,JGSST,4).EQ.1)GO TO 4
        ELSEIF(NGSST.EQ.3)THEN
*END?
          IF(MLEQS(JFI1,JGSST,3).EQ.1)GO TO 4
        ELSEIF(NGSST.EQ.2)THEN
         IF(MLEQS(JEXIT,JGSST,1).EQ.1)GO TO 4
        ELSEIF(NGSST.EQ.1)THEN
         IF(MLEQS(JQUIT,JGSST,1).EQ.1)GO TO 4
        ENDIF
      ENDIF
      KPRO=CSPMFS(IBCODE)
      IF(KPRO.EQ.0)GO TO 2
*--      CALL CSTLOG(JPMB)
      KEYRD=0
      IF(KPRO.NE.3)GO TO 2
      CALL CSLINK
      I=CSINT1(IBCODE)
      IF(I.NE.2)GO TO 1
  4   IF(MNRECU.EQ.2)ISTFIL=ISVFIL
      MNRECU=MNRECU-1
      KEYRD=0
      ISTATE=1
      END
