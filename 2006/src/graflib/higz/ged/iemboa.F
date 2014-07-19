*
* $Id: iemboa.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iemboa.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.21  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMBOA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*80 CHVAL(4)
*.______________________________________
*
      TITLE(3)='BOX ATTRIBUTES'
      CHITEM(1,3)='Box interior style'
      CHITEM(2,3)='Box style index'
      CHITEM(3,3)='Box color index'
      CHITEM(4,3)='Border'
*
      CHDEF(1)=CHBOIS
      CALL IZITOC(ICBOSI,CHDEF(2))
      CHDEF(3)=CHBOC
      CHDEF(4)=CHBOBO
*
      IPLACE=3
      CALL IGREQ(270,0,4,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      CALL IGFIRS(CHBOIS,CHVAL(1))
      ICBOIS=IGIFAI(CHVAL(1))
      CALL IZCTOI(CHVAL(2),ICBOSI)
      CALL IGFIRS(CHBOC,CHVAL(3))
      ICBOCI=IGICOL(CHVAL(3))
      CALL IGFIRS(CHBOBO,CHVAL(4))
      IF(CHVAL(4).EQ.'No')THEN
         ICBOBO=0
      ELSE
         ICBOBO=1
      ENDIF
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL ISFAIS(ICBOIS)
      CALL ISFASI(ICBOSI)
      CALL ISFACI(ICBOCI)
      CALL IGSET('BORD',FLOAT(ICBOBO))
*
      END
