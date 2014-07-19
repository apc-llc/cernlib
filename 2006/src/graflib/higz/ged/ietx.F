*
* $Id: ietx.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: ietx.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  13.29.50  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IETX(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/higed.inc"
#include "higz/hiflag.inc"
*.______________________________________
*
   10 CALL IEMTXA(IPLACE,ICHOIC)
*
      IF(ICHOIC.EQ.-1000)THEN
         ICF=2
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      X(1)=RQUEST(13)
      Y(1)=RQUEST(14)
*
      LN=LQ(LPICD-1)
      DO 20 I=1,IQ(LPICD+8)
         IF(LN.EQ.0)GOTO 10
         IF(IQ(LN+1).NE.0.AND.IQ(LN+1).NE.1)THEN
            CALL ISELNT(IQ(LN+1))
         ENDIF
         LN=LQ(LN)
   20 CONTINUE
*
      IF(IPLACE.EQ.1)THEN
         ICF=1
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      IF(IPLACE.EQ.2)THEN
         ICF=2
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL ISELNT(1)
      CALL IGGRID(X(1),Y(1))
      CALL IGSET('TANG',RCTXAN)
      CALL ISCHH(RCTXSI)
      CALL ISTXFP(ICTXFT,ICTXPR)
      CALL ISTXCI(ICTXCI)
      CALL ISTXAL(ICTXAH,ICTXAV)
      CALL ITX(X(1),Y(1),CCTX)
*
      LN=LQ(LPICD-1)
      DO 30 I=1,IQ(LPICD+8)
         IF(LN.EQ.0)GOTO 10
         IF(IQ(LN+1).NE.0.AND.IQ(LN+1).NE.1)THEN
            CALL ISELNT(IQ(LN+1))
         ENDIF
         LN=LQ(LN)
   30 CONTINUE
      GOTO 10
*
      END
