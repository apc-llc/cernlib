*
* $Id: csexec.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: csexec.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 16/02/94  15.52.35  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSEXEC(TEXT,KERR)
***--------------------------------
*        routine like cspaus
***--------------------------------
       INTEGER CSPMFS,CSINT1
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/cslun.inc"
#include "comis/csichv.inc"
         COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
         CHARACTER *(*) TEXT
         CHARACTER*8 PRMAIN,QUIT*4,EXIT*4
         DATA PRMAIN/'*MAINE'/
         DATA QUIT/'QUIT'/, EXIT/'EXIT'/
      MDEDIT=1
      KERR=0
      IF(JPMB.NE.0)CALL CSLFRE(JPMB)
      JPMC=0
      NC=LENOCC(TEXT)
      CALL CSPUTL(JPMC,TEXT,NC,0)
      JPMB=JPMC
      KEYRD=-1
      JQUIT=MJSCHA(QUIT)
      JEXIT=MJSCHA(EXIT)
      CALL CCOPYS(MJSCHA(PRMAIN),JSMAIN,8)
  2   ISTATE=5
      CALL CSRD(JGSST,NGSST)
      IF (NGSST .LT. 0) GO TO 4
      K=MKBLAN(JGSST,NGSST)
      IF(NGSST.EQ.0)GO TO 4
      IF(K.EQ.ICHSX)GO TO 4
      IF(K.EQ.ICHSE)THEN
*   !DIRECTIVE
        JGSST=JGSST+1
        NGSST=NGSST-1
        IF(KEYRD.NE.-1)THEN
          KEYRD=0
        ENDIF
        CALL CSPDIR(KERR)
        IF(ISTFIL.EQ.0 .OR. KERR.NE.0)GO TO 4
        KEYRD=0
  3     ISTATE=5
        CALL CSRD(JGSST,NGSST)
        IF (NGSST .LT. 0) GO TO 4
        K=MKBLAN(JGSST,NGSST)
        IF(NGSST.EQ.0)GO TO 4
        IF(K.EQ.ICHSX)GO TO 4
        IF(K.EQ.ICHSE)THEN
*   !DIRECTIVE
          JGSST=JGSST+1
          NGSST=NGSST-1
          IF(KEYRD.NE.-1)THEN
            KEYRD=0
          ENDIF
          CALL CSPDIR(KERR)
          IF(KERR.NE.0)GO TO 4
          GO TO 3
        ENDIF
        IF(NGSST.LE.4)THEN
          IF(NGSST.EQ.4)THEN
*QUIT? EXIT
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
        IF(KPRO.EQ.0)THEN
          KERR=1
          GO TO 4
        ENDIF
        KEYRD=0
        IF(KPRO.NE.3)GO TO 3
        CALL CSLINK
        I=CSINT1(IBCODE)
        IF(I.NE.2)GO TO 3
        GO TO 4
      ENDIF
      IF(NGSST.LE.4)THEN
        IF(NGSST.EQ.4)THEN
*QUIT?
          IF(MLEQS(JQUIT,JGSST,4).EQ.1)GO TO 4
          IF(MLEQS(JEXIT,JGSST,4).EQ.1)GO TO 4
        ELSEIF(NGSST.EQ.3)THEN
*END?
          IF(MLEQS(JFI1,JGSST,3).EQ.1)GO TO 4
        ELSEIF(NGSST.EQ.2)THEN
         IF(MLEQS(JEXIT,JGSST,2).EQ.1)GO TO 4
        ELSEIF(NGSST.EQ.1)THEN
         IF(MLEQS(JQUIT,JGSST,1).EQ.1)GO TO 4
        ENDIF
      ENDIF
      KPRO=CSPMFS(IBCODE)
      IF(KPRO.EQ.0)THEN
        KERR=1
        GO TO 4
      ENDIF
      KEYRD=0
      IF(KPRO.NE.3)GO TO 4
      CALL CSLINK
      I=CSINT1(IBCODE)
  4   KEYRD=0
      ISTATE=1
      MDEDIT=0
      IF(ISTFIL.NE.0)THEN
        CLOSE(LUNFIL)
        ISTFIL=0
      ENDIF
      END
