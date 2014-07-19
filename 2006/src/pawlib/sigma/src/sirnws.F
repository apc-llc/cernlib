*
* $Id: sirnws.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: sirnws.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
       SUBROUTINE SIRNWS
C SIRNWS
#include "sigma/sicipa.inc"
      COMMON/SICIFI/ISYSF
#include "sigma/sicsig.inc"
#include "sigma/sigkq.inc"
#include "sigma/sigc.inc"
#include "sigma/sicst4.inc"
C
      COMMON/PAWSIG/CHCOM
      CHARACTER*500 CHCOM
      CHARACTER KOLN*500,CN*2
      IF (IPASW.EQ.-1) GOTO 100
      CALL SITRAX(' SIRNWS  ')
10000 FORMAT(' SIGMA>',I5,'. ')
      LINE=LINE+1
   10 CONTINUE
      IF(IPASW.NE.2) GOTO 20
      KOLTEX(LINE)(1:5)='!STOP'
      KOLN=KOLTEX(LINE)
*      PRINT *,KOLN
      LENSTR=5
      IPASW=0
      GOTO 60
   20  CONTINUE
      IF(IPASW.EQ.1)GOTO 90
      IF(ISYSF.NE.0)GOTO 30
      KOLTEX(LINE)='Q'
   30 CONTINUE
      CALL CLTOU(KOLTEX(LINE))
      IF(INDEX(KQAT,KOLTEX(LINE)(1:1)).EQ.0) GOTO  40
      CALL SIREFR
      GOTO 10
   40 CONTINUE
      KOLN=KOLTEX(LINE)
      IF(KOLN(1:1).EQ.']') KOLTEX(LINE)(1:5)='!STOP'
   60    CN=KOLN(1:2)
      LENSTR=LENOCC(KOLN)
      IF(SITRAK(10)+SITRAK(41).EQ.0)GO TO 70
      PRINT 10100,LINE,LENSTR
10100 FORMAT(' LINE= ',I4,' LENSTR=',I5)
      PRINT 10200,KOLTEX(LINE)
10200 FORMAT(1X,A72)
   70 CONTINUE
      RETURN
   90 CONTINUE
      KOLTEX(LINE)=CSTA1
      IPASW=2
      GOTO 30
10400 FORMAT (A72)
C modified for PAWFCU
  100 CONTINUE
      NCH=LENOCC(CHCOM)
      KOLTEX(LINE)=CHCOM
      IF(SITRAK(42).NE.0) PRINT 10200,KOLTEX(LINE)
      IPASW=2
      LENSTR=NCH
C end of modification
  999 END
