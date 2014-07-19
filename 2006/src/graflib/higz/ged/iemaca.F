*
* $Id: iemaca.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iemaca.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.21  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMACA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*32 CHVAL(5)
*.______________________________________
*
      TITLE(3)='ARC ATTRIBUTES'
      CHITEM(1,3)='Fill area interior style'
      CHITEM(2,3)='Fill area style index'
      CHITEM(3,3)='Arc border'
      CHITEM(4,3)='Line color'
      CHITEM(5,3)='Fill area color'
*
      CHDEF(1)=CHACIS
      CALL IZITOC(ICACFS,CHDEF(2))
      CHDEF(3)=CHACBO
      CHDEF(4)=CHACLC
      CHDEF(5)=CHACFC
*
      IPLACE=3
      CALL IGREQ(270,0,5,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL IGFIRS(CHACIS,CHVAL(1))
      ICACFI=IGIFAI(CHVAL(1))
      CALL IZCTOI(CHVAL(2),ICACFS)
      CALL IGFIRS(CHACBO,CHVAL(3))
      IF(CHVAL(3).EQ.'No')THEN
         ICACBO=0
      ELSE
         ICACBO=1
      ENDIF
      CALL IGFIRS(CHACLC,CHVAL(4))
      ICACLC=IGICOL(CHVAL(4))
      CALL IGFIRS(CHACFC,CHVAL(5))
      ICACFC=IGICOL(CHVAL(5))
*
      CALL ISFAIS(ICACFI)
      CALL ISFASI(ICACFS)
      CALL ISFACI(ICACFC)
      CALL ISPLCI(ICACLC)
      CALL IGSET('BORD',FLOAT(ICACBO))
*
      END
