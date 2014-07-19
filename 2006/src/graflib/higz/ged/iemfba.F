*
* $Id: iemfba.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iemfba.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.22  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMFBA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*80 CHVAL(4)
*.______________________________________
*
      TITLE(3)='FRAME BOX ATTRIBUTES'
      CHITEM(1,3)='Frame Box interior style'
      CHITEM(2,3)='Frame Box style index'
      CHITEM(3,3)='Frame Box color index'
      CHITEM(4,3)='Frame Box Border'
*
      CHDEF(1)=CHFBIS
      CALL IZITOC(ICFBSI,CHDEF(2))
      CHDEF(3)=CHFBC
      CHDEF(4)=CHFBBO
*
      IPLACE=3
      CALL IGREQ(280,0,4,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      CALL IGFIRS(CHFBIS,CHVAL(1))
      ICFBIS=IGIFAI(CHVAL(1))
      CALL IZCTOI(CHVAL(2),ICFBSI)
      CALL IGFIRS(CHFBC,CHVAL(3))
      ICFBCI=IGICOL(CHVAL(3))
      CALL IGFIRS(CHFBBO,CHVAL(4))
      IF(CHVAL(4).EQ.'No')THEN
         ICFBBO=0
      ELSE
         ICFBBO=1
      ENDIF
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL ISFAIS(ICFBIS)
      CALL ISFASI(ICFBSI)
      CALL ISFACI(ICFBCI)
      CALL IGSET('BORD',FLOAT(ICFBBO))
*
      END
