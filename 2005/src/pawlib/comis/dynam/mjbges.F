*
* $Id: mjbges.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mjbges.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.52.46  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MJBGES(J1,N1,J2,N2)
#include "comis/csichv.inc"
      MJBGES=0
      I1=-1
      I2=-1
    1 I1=I1+1
      I2=I2+1
      IF(I2.GE.N2) GO TO 6
      IF(I1.GE.N1) RETURN
      KCH2=MKCHAR(J2+I2)
      IF(KCH2.EQ.ICHBLN) GO TO 3
      KCH1=MKCHAR(J1+I1)
    2 IF(KCH1.NE.KCH2) RETURN
      GO  TO 1
    3 KCH1=MKCHAR(J1+I1)
      IF(KCH1.NE.KCH2)  RETURN
      I1=I1+1
      I2=I2+1
      IF(I2.GE.N2) GO TO 5
      IF(I1.GE.N1) RETURN
      KCH2=MKCHAR(J2+I2)
      IF(KCH2 .EQ. ICHBLN) GO TO 3
    4 KCH1=MKCHAR(J1+I1)
      IF(KCH1 .NE. ICHBLN) GO TO 2
      I1=I1+1
      IF(I1.GE.N1) RETURN
      GO TO 4
    5 KCH1=MKCHAR(J1+I1)
      IF(KCH1 .NE. ICHBLN) GO TO 6
      I1=I1+1
      GO  TO 5
    6 MJBGES=J1+I1
      RETURN
      END
