*
* $Id: iempma.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iempma.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.22  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMPMA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*80 CHVAL(3)
*.______________________________________
*
      TITLE(3)='POLYMARKER ATTRIBUTES'
      CHITEM(1,3)='Polymarker type'
      CHITEM(2,3)='Polymarker color index'
      CHITEM(3,3)='Polymarker scale factor'
*
      CALL IZITOC(ICMK,CHDEF(1))
      CHDEF(2)=CHPMC
      CALL IZRTOC(RCMKSC,CHDEF(3))
*
      IPLACE=3
      CALL IGREQ(240,0,3,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      CALL IZCTOI(CHVAL(1),ICMK)
      CALL IGFIRS(CHPMC,CHVAL(2))
      ICPMCI=IGICOL(CHVAL(2))
      CALL IZCTOR(CHVAL(3),RCMKSC)
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL ISMK(ICMK)
      CALL ISPMCI(ICPMCI)
      CALL ISMKSC(RCMKSC)
*
      END
