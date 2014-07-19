*
* $Id: iefbox.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iefbox.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.47.25  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEFBOX(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
#include "higz/hiatt.inc"
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*4 CHVAL(15)
*.______________________________________
*
      ICHOIO=0
 201  IPLACE=2
      CALL IGREQ(21,3,16,IPLACE,ICHOIC,CHVAL)
      IF(ICHOIC.EQ.-1000)GOTO 201
*
 203  X(1)=RQUEST(13)
      Y(1)=RQUEST(14)
      NTLOC=IQUEST(10)
*
      IF(IPLACE.EQ.3)GOTO 201
*
      IF(IPLACE.EQ.1)THEN
         ICF=1
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      IF(ICHOIC.EQ.-1)THEN
         CALL IEMFBA(IPLACE,ICHOIC)
         GOTO 203
      ENDIF
*
      IF(ICHOIC.EQ.-2)THEN
         CALL IGCLES
         CALL IZPICT(EDIPIC,'D')
         CALL IEGRID
         GOTO 201
      ENDIF
*
      IF(ICHOIC.EQ.-3)THEN
         CALL IZUNDO
         GOTO 201
      ENDIF
*
      IF(ICHOIC.GE.1)THEN
         ICF=2
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      CALL ISELNT(NTLOC)
      CALL IGGRID(X(1),Y(1))
      CALL IGPLOT(X(1),Y(1))
      CALL IZSAV
*
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISFAIS(0)
      CALL ISFACI(1)
      CALL IGSET('BORD',1.)
      I=2
 202  CALL IGLOC(51,NTLOC,IBN,XNDC,YNDC,X(I),Y(I))
      IF(NTLOC.NE.INTR)GOTO 202
      CALL IGGRID(X(I),Y(I))
      I=I+1
      CALL IGBOX(X(1),X(2),Y(1),Y(2))
 204  CALL IGLOC(1,NTLOC,IBN,XNDC,YNDC,X(I),Y(I))
      IF(NTLOC.NE.INTR)GOTO 204
      CALL IGGRID(X(I),Y(I))
      I=I+1
      CALL IGPLOT(X(3),Y(3))
 205  CALL IGLOC(51,NTLOC,IBN,XNDC,YNDC,X(I),Y(I))
      IF(NTLOC.NE.INTR)GOTO 205
      CALL IGGRID(X(I),Y(I))
      CALL IGBOX(X(3),X(4),Y(3),Y(4))
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      CALL ISFAIS(ICFBIS)
      CALL ISFASI(ICFBSI)
      CALL ISFACI(ICFBCI)
      CALL IGSET('BORD',FLOAT(ICBOBO))
      CALL IGFBOX(X(1),X(2),Y(1),Y(2),X(3),X(4),Y(3),Y(4))
      GOTO 201
*
      END
