*
* $Id: ieaxis.F,v 1.1.1.1 1996/02/14 13:10:21 mclareni Exp $
*
* $Log: ieaxis.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.47.24  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEAXIS(ICF,ICHOIO)
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
         CALL IEMAXA(IPLACE,ICHOIC)
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
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISELNT(NTLOC)
      CALL IGGRID(X(1),Y(1))
      CALL IGPLOT(X(1),Y(1))
      CALL IGLOC(41,NTLOC,IBN,XNDC,YNDC,X(2),Y(2))
      IF(IBN.EQ.1)THEN
         IF(NTLOC.NE.INTR)GOTO 202
         CALL IGGRID(X(2),Y(2))
         CALL IEALPT(X,Y)
         CALL IGLINE(X(1),X(2),Y(1),Y(2))
         GOTO 202
      ELSE
         GOTO 201
      ENDIF
*
 202  ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL IZSAV
      CALL ISPLCI(0)
      CALL ISLN(1)
      CALL IPL(2,X,Y)
      CALL IZSET
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISELNT(NTLOC)
      CALL IGSET('TMSI',RCAXTS)
      CALL IGSET('LASI',RCAXLS)
      CALL IGSET('LAOF',RCAXLO)
      CALL ISLN(ICAXLN)
      CALL ISPLCI(ICAXLC)
      CALL ISTXFP(ICAXLF,ICAXLP)
      CALL IGAXIS(X(1),X(2),Y(1),Y(2),RCAXW1,RCAXW2,ICAXND,CHOPAX)
      GOTO 201
*
      END
