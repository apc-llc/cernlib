*
* $Id: iemfaa.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iemfaa.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.21  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMFAA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*80 CHVAL(3)
*.______________________________________
*
      TITLE(3)='FILL AREA ATTRIBUTES'
      CHITEM(1,3)='Fill area interior style'
      CHITEM(2,3)='Fill area style index'
      CHITEM(3,3)='Fill area color index'
*
      CHDEF(1)=CHFAIS
      CALL IZITOC(ICFASI,CHDEF(2))
      CHDEF(3)=CHFAC
*
      IPLACE=3
      CALL IGREQ(220,0,3,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      CALL IGFIRS(CHFAIS,CHVAL(1))
      ICFAIS=IGIFAI(CHVAL(1))
      CALL IZCTOI(CHVAL(2),ICFASI)
      CALL IGFIRS(CHFAC,CHVAL(3))
      ICFACI=IGICOL(CHVAL(3))
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL ISFAIS(ICFAIS)
      CALL ISFASI(ICFASI)
      CALL ISFACI(ICFACI)
*
      END
