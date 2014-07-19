*
* $Id: iepl.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iepl.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.47.26  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEPL(ICF,ICHOIO)
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
         CALL IEMPLA(IPLACE,ICHOIC)
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
      I=2
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISELNT(NTLOC)
      CALL IGGRID(X(1),Y(1))
      CALL IGPLOT(X(1),Y(1))
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
 202  CALL IGLOC(41,NTLOC,IBN,XNDC,YNDC,X(I),Y(I))
      IF(IBN.EQ.1)THEN
         IF(NTLOC.NE.INTR)GOTO 202
         CALL IGGRID(X(I),Y(I))
         CALL IEALPT(X(I-1),Y(I-1))
         CALL IGLINE(X(I-1),X(I),Y(I-1),Y(I))
         I=I+1
         GOTO 202
      ELSE
         IF(I.LE.2)GOTO 201
         ZFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         CALL IZSAV
         CALL IGLINE(X(I-1),X(1),Y(I-1),Y(1))
         CALL ISPLCI(0)
         CALL ISLN(1)
         X(I)=X(1)
         Y(I)=Y(1)
         CALL IPL(I,X,Y)
         CALL IZSET
         ZFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         GFLAG=.TRUE.
         CALL ISLN(ICLN)
         CALL ISLWSC(RCLWSC)
         CALL ISPLCI(ICPLCI)
         CALL IPL(I-1,X,Y)
         GOTO 201
      ENDIF
*
      END
