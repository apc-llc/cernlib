*
* $Id: iemhia.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iemhia.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.22  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMHIA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*32 CHVAL(8)
*.______________________________________
*
      TITLE(3)='HISTOGRAM ATTRIBUTES'
      CHITEM(1,3)='CHOPT'
      CHITEM(2,3)='Bars offset'
      CHITEM(3,3)='Bars width'
      CHITEM(4,3)='Histogram line type'
      CHITEM(5,3)='Histogram line color'
      CHITEM(6,3)='Histogram fill color'
      CHITEM(7,3)='Histogram interior style'
      CHITEM(8,3)='Histogram style index'
*
      CHDEF(1)=CHOPHI
      CALL IZRTOC(RCBOF,CHDEF(2))
      CALL IZRTOC(RCBWD,CHDEF(3))
      CALL IZITOC(ICHILN,CHDEF(4))
      CHDEF(5)=CHHIPC
      CHDEF(6)=CHHIFC
      CHDEF(7)=CHHIIS
      CALL IZITOC(ICHIFS,CHDEF(8))
*
      IPLACE=3
      CALL IGREQ(700,0,8,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL CLTOU(CHVAL(1))
      CHOPHI=CHVAL(1)
      CALL IZCTOR(CHVAL(2),RCBOF)
      CALL IZCTOR(CHVAL(3),RCBWD)
      CALL IZCTOI(CHVAL(4),ICHILN)
      CALL IGFIRS(CHHIPC,CHVAL(5))
      ICHIPC=IGICOL(CHVAL(5))
      CALL IGFIRS(CHHIFC,CHVAL(6))
      ICHIFC=IGICOL(CHVAL(6))
      CALL IGFIRS(CHHIIS,CHVAL(7))
      ICHIFI=IGIFAI(CHVAL(7))
      IF(ICHIFI.NE.0.AND.INDEX(CHOPHI,'F').EQ.0)THEN
         CHOPHI=CHOPHI(1:LENOCC(CHOPHI))//'F'
      ENDIF
      CALL IZCTOI(CHVAL(8),ICHIFS)
*
      CALL IGSET('BARO',RCBOF)
      CALL IGSET('BARW',RCBWD)
      CALL ISLN(ICHILN)
      CALL ISPLCI(ICHIPC)
      CALL ISFACI(ICHIFC)
      CALL ISFAIS(ICHIFI)
      CALL ISFASI(ICHIFS)
*
      END
