*
* $Id: iempla.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iempla.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/01/89  11.49.22  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMPLA(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*80 CHVAL(3)
*.______________________________________
*
      TITLE(3)='POLYLINE ATTRIBUTES'
      CHITEM(1,3)='Polyline style'
      CHITEM(2,3)='Line width scale factor'
      CHITEM(3,3)='Polyline color index'
*
      CALL IZITOC(ICLN,CHDEF(1))
      CALL IZRTOC(RCLWSC,CHDEF(2))
      CHDEF(3)=CHPLC
*
      IPLACE=3
      CALL IGREQ(230,0,3,IPLACE,ICHOIC,CHVAL)
      ICHOIO=ICHOIC
      ICF=IPLACE
*
      CALL IZCTOI(CHVAL(1),ICLN)
      CALL IZCTOR(CHVAL(2),RCLWSC)
      CALL IGFIRS(CHPLC,CHVAL(3))
      ICPLCI=IGICOL(CHVAL(3))
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL ISLN(ICLN)
      CALL ISLWSC(RCLWSC)
      CALL ISPLCI(ICPLCI)
*
      END
