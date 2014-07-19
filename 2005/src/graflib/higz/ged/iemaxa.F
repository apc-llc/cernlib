*
* $Id: iemaxa.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iemaxa.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.21  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMAXA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*80 CHVAL(11)
*.______________________________________
*
      TITLE(3)='AXIS ATTRIBUTES'
      CHITEM(1,3)='CHOPT'
      CHITEM(2,3)='WMIN'
      CHITEM(3,3)='WMAX'
      CHITEM(4,3)='Number of divisions'
      CHITEM(5,3)='Tick marks size'
      CHITEM(6,3)='Labels size'
      CHITEM(7,3)='Labels offset'
      CHITEM(8,3)='Axis color'
      CHITEM(9,3)='Axis line type'
      CHITEM(10,3)='Labels font'
      CHITEM(11,3)='Labels precision'
*
      CHDEF(1)=CHOPAX
      CHDEF(2)=' '
      CHDEF(3)=' '
      CHDEF(4)=' '
      CALL IZRTOC(RCAXTS,CHDEF(5))
      CALL IZRTOC(RCAXLS,CHDEF(6))
      CALL IZRTOC(RCAXLO,CHDEF(7))
      CHDEF(8)=CHAXLC
      CALL IZITOC(ICAXLN,CHDEF(9))
      CALL IZITOC(ICAXLF,CHDEF(10))
      CALL IZITOC(ICAXLP,CHDEF(11))
*
      IPLACE=3
      CALL IGREQ(260,0,11,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      CALL CLTOU(CHVAL(1))
      CHOPAX=CHVAL(1)
      CALL IZCTOR(CHVAL(2),X1)
      CALL IZCTOR(CHVAL(3),X2)
      IF(X1.NE.X2)THEN
         RCAXW1=X1
         RCAXW2=X2
      ENDIF
      IF(CHVAL(4).NE.' ')THEN
         CALL IZCTOI(CHVAL(4),ICAXND)
      ENDIF
      CALL IZCTOR(CHVAL(5),RCAXTS)
      CALL IZCTOR(CHVAL(6),RCAXLS)
      CALL IZCTOR(CHVAL(7),RCAXLO)
      CALL IGFIRS(CHAXLC,CHVAL(8))
      ICAXLC=IGICOL(CHVAL(8))
      CALL IZCTOI(CHVAL(9),ICAXLN)
      CALL IZCTOI(CHVAL(10),ICAXLF)
      CALL IZCTOI(CHVAL(11),ICAXLP)
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL IGSET('TMSI',RCAXTS)
      CALL IGSET('LASI',RCAXLS)
      CALL IGSET('LAOF',RCAXLO)
      CALL ISLN(ICAXLN)
      CALL ISPLCI(ICAXLC)
      CALL ISTXFP(ICAXLF,ICAXLP)
*
      END
