*
* $Id: iemtxa.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iemtxa.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.22  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMTXA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*32 CHVAL(8)
*.______________________________________
*
      TITLE(3)='TEXT'
      CHITEM(1,3)='Text to be drawn'
      CHITEM(2,3)='Text size'
      CHITEM(3,3)='Text angle'
      CHITEM(4,3)='Text font'
      CHITEM(5,3)='Text precision'
      CHITEM(6,3)='Text color'
      CHITEM(7,3)='Horizontal alignment'
      CHITEM(8,3)='Vertical alignment'
*
      CHDEF(1)=CCTX
      CALL IZRTOC(RCTXSI,CHDEF(2))
      CALL IZRTOC(RCTXAN,CHDEF(3))
      CALL IZITOC(ICTXFT,CHDEF(4))
      CALL IZITOC(ICTXPR,CHDEF(5))
      CHDEF(6)=CHTXC
      CALL IZITOC(ICTXAH,CHDEF(7))
      CALL IZITOC(ICTXAV,CHDEF(8))
*
      IPLACE=3
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISELNT(1)
      CALL IGREQ(1001,0,8,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      CALL IGFIRS(CHTXC,CHVAL(6))
      ICTXCI=IGICOL(CHVAL(6))
      CCTX=CHVAL(1)
      CALL IZCTOR(CHVAL(2),RCTXSI)
      CALL IZCTOR(CHVAL(3),RCTXAN)
      CALL IZCTOI(CHVAL(4),ICTXFT)
      CALL IZCTOI(CHVAL(5),ICTXPR)
      CALL IZCTOI(CHVAL(7),ICTXAH)
      CALL IZCTOI(CHVAL(8),ICTXAV)
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL IGSET('TANG',RCTXAN)
      CALL ISCHH(RCTXSI)
      CALL ISTXFP(ICTXFT,ICTXPR)
      CALL ISTXCI(ICTXCI)
      CALL ISTXAL(ICTXAH,ICTXAV)
*
      END
