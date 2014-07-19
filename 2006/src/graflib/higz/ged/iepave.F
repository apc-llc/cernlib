*
* $Id: iepave.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iepave.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.47.25  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEPAVE(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
#include "higz/hiatt.inc"
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*32 CHVAL(15)
*.______________________________________
*
      ICHOIO=0
   10 IPLACE=2
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      NTSAV=INTR
      CALL ISELNT(1)
      CALL IGREQ(21,3,16,IPLACE,ICHOIC,CHVAL)
      IF(ICHOIC.EQ.-1000)GOTO 10
*
   20 X(1)=RQUEST(13)
      Y(1)=RQUEST(14)
      CALL IGGRID(X(1),Y(1))
      NTLOC=IQUEST(10)
*
   30 IF(IPLACE.EQ.1)THEN
         ICF=1
         ICHOIO=ICHOIC
         CALL ISELNT(NTSAV)
         RETURN
      ENDIF
*
      IF(IPLACE.EQ.3)GOTO 10
*
      IF(ICHOIC.EQ.-1)THEN
         TITLE(3)='PAVING-BLOCK ATTRIBUTES'
         CHITEM(1,3)='Boxe style'
         CHITEM(2,3)='Frame style'
         CHITEM(3,3)='Frame size'
         CHITEM(4,3)='Options'
         CALL IZITOC(ICPBST,CHDEF(1))
         CALL IZITOC(ICPFST,CHDEF(2))
         CALL IZRTOC(RCDZPV,CHDEF(3))
         CHDEF(4)=CHOPPV
         IPLACE=3
         CALL IGREQ(298,0,4,IPLACE,ICHOIC,CHVAL)
         ICHOIO=ICHOIC
         ICF=IPLACE
         CALL IZCTOI(CHVAL(1),ICPBST)
         CALL IZCTOI(CHVAL(2),ICPFST)
         CALL IZCTOR(CHVAL(3),RCDZPV)
         CHOPPV=CHVAL(4)
         GOTO 20
      ENDIF
*
      IF(ICHOIC.EQ.-2)THEN
         CALL IGCLES
         CALL IZPICT(EDIPIC,'D')
         CALL IEGRID
         GOTO 10
      ENDIF
*
      IF(ICHOIC.EQ.-3)THEN
         CALL IZUNDO
         GOTO 10
      ENDIF
*
      IF(ICHOIC.GE.1)THEN
         ICF=2
         ICHOIO=ICHOIC
         CALL ISELNT(NTSAV)
         RETURN
      ENDIF
*

      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISELNT(NTLOC)
      CALL IGGRID(X(1),Y(1))
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL IGPLOT(X(1),Y(1))
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
   40 CALL IGLOC(51,NTLOC,IBN,XNDC,YNDC,X(2),Y(2))
      IF(NTLOC.NE.INTR)GOTO 40
      CALL IGGRID(X(2),Y(2))
      CALL IGPAVE(X(1),X(2),Y(1),Y(2),RCDZPV,ICPBST,ICPFST,CHOPPV)
      GOTO 10
*
      END
