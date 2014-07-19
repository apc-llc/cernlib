*
* $Id: hframe.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hframe.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.42.37  by  Julian Bunn
*-- Author :
      SUBROUTINE HFRAME(IX,XMIN,FACTOR,W5)
*.==========>
*.           PRINTS STATISTICS FOR SCATTER PLOTS AND TABLES
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      CHARACTER*9 XNUMB
      COMMON/HEXPO/ISI,IE1,IE2,NBS,FACT
      COMMON/HFORM/IA(127),IDU
*.___________________________________________
      FA=1.
      IF(NBS.GE.32.AND.I3.NE.0)FA=FACT
      CALL VBLANK(IA,NCOLPA)
      IA(61)=IDG(19)
      IA(71)=IDG(19)
      NCX=IQ(LCID+KNCX)
      NCY=IQ(LCID+KNCY)
      W1=HCXY(0,NCY+1,1)*FA
      W2=0.
      W8=0.
      DO 1 I=1,NCX
         W2=W2+HCXY(I,NCY+1,1)*FA
         W8=W8+HCXY(I,0,1)*FA
   1  CONTINUE
      W3=HCXY(NCX+1,NCY+1,1)*FA
      W4=0.
      W6=0.
      DO 2 I=1,NCY
         W4=W4+HCXY(0,I,1)*FA
         W6=W6+HCXY(NCX+1,I,1)*FA
   2  CONTINUE
      W7=HCXY(0,0,1)*FA
      W9=HCXY(NCX+1,0,1)*FA
      W5=W5-W1-W2-W3-W4-W6-W7-W8-W9
*
*             FIRST LINE
*
      CALL HBCD(W1,9,IA(52))
      CALL HBCD(W2,9,IA(62))
      CALL HBCD(W3,9,IA(72))
      IA(2)=IDG(37)
      CALL HFORMA(1)
*
*             SECOND LINE
*
      CALL VBLANK(IA(4),NCOLPA-3)
      CALL UCTOH('ENTRIES =',IA(4),1,9)
*
      CALL HBCDI(IX,9,IA(13))
      CALL UCTOH('PLOT',IA(41),1,4)
*
      CALL VFILL(IA(52),29,IDG(39))
      IA(61)=IDG(19)
      IA(71)=IDG(19)
      CALL HFORMA(1)
*
*             LINE 3
*
      CALL VBLANK(IA(4),NCOLPA-3)
      CALL UCTOH('SATURATION  AT=     INFINITY ',IA(4),1,28)
*
      IF(NBS.LT.32)CALL HBCDI(MAXBIT(NBS),9,IA(23))
*
      CALL HBCD(W4,9,IA(52))
      IA(61)=IDG(19)
      IA(71)=IDG(19)
      CALL HBCD(W5,9,IA(62))
      CALL HBCD(W6,9,IA(72))
      CALL HFORMA(1)
*
*             LINE 4
*
      CALL VBLANK(IA(4),NCOLPA-3)
      IF(I3.NE.0)GO TO 20
      CALL UCTOH('SCALE  .,+,2,3,.,., A,B,',IA(4),1,24)
  20  CALL UCTOH('STATISTICS',IA(39),1,10)
*
      CALL VFILL(IA(52),29,IDG(39))
      IA(61)=IDG(19)
      IA(71)=IDG(19)
      CALL HFORMA(1)
*
*             LINE 5
*
      CALL VBLANK(IA(4),NCOLPA-3)
      IF(I2.NE.0)GO TO 35
      IF(NBS.LT.32)GO TO 40
      IF(IE2.EQ.IDG(41))GO TO 40
      CALL UCTOH('CONTENTS *10**',IA(4),1,14)
      IA(18)=ISI
      IA(19)=IE1
      IA(20)=IE2
      GO TO 40
*
  35  CALL UCTOH('STEP =',IA(4),1,6)
      WRITE(XNUMB,'(G9.3)')FACTOR
      CALL UCTOH1(XNUMB,IA(10),9)
      CALL UCTOH('* MINIMUM=',IA(20),1,10)
      WRITE(XNUMB,'(G9.3)')XMIN
      CALL UCTOH1(XNUMB,IA(30),9)
      IF(XMIN.EQ.0.)IA(30)=IDG(1)
  40  CALL HBCD(W7,9,IA(52))
      IA(61)=IDG(19)
      IA(71)=IDG(19)
      CALL HBCD(W8,9,IA(62))
      CALL HBCD(W9,9,IA(72))
      CALL HFORMA(1)
*
      END
