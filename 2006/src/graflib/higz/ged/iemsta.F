*
* $Id: iemsta.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iemsta.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/05 01/03/89  11.51.52  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMSTA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*32 CHVAL(7)
*.______________________________________
*
      TITLE(3)='SOFTWARE TEXT'
      CHITEM(1,3)='Text to be drawn'
      CHITEM(2,3)='Text size'
      CHITEM(3,3)='Text angle'
      CHITEM(4,3)='Text shift'
      CHITEM(5,3)='Text color index'
      CHITEM(6,3)='Text pass'
      CHITEM(7,3)='CHOPT'
*
      CHDEF(1)=CCST
      CALL IZRTOC(RCSTSI,CHDEF(2))
      CALL IZRTOC(RCSTAN,CHDEF(3))
      CALL IZRTOC(RCSTSH,CHDEF(4))
      CHDEF(5)=CHSTC
      CALL IZITOC(ICSTPA,CHDEF(6))
      CHDEF(7)=CHOPST
*
      IPLACE=3
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISELNT(1)
      CALL IGREQ(1000,0,7,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      CALL IGFIRS(CHSTC,CHVAL(5))
      ICSTCI=IGICOL(CHVAL(5))
      CCST=CHVAL(1)
      CALL IZCTOR(CHVAL(2),RCSTSI)
      CALL IZCTOR(CHVAL(3),RCSTAN)
      CALL IZCTOR(CHVAL(4),RCSTSH)
      CALL IZCTOI(CHVAL(6),ICSTPA)
      CALL CLTOU(CHVAL(7))
      CHOPST=CHVAL(7)
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL IGSET('CSHI',RCSTSH)
      CALL IGSET('PASS',FLOAT(ICSTPA))
      CALL ISTXCI(ICSTCI)
*
      END
