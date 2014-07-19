*
* $Id: mjfges.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mjfges.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.54.02  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MJFGES(J1,N1,J2,N2)
#include "comis/csichv.inc"
      MJFGES=0
      I1=-1
      I2=-1
    1 I1=I1+1
      I2=I2+1
      IF(I2.GE.N2) GO TO 4
      IF(I1.GE.N1)  RETURN
      KCH1=MKCHAR(J1+I1)
      KCH2=MKCHAR(J2+I2)
    2 IF(KCH1.EQ.KCH2) GO TO 1
      IF(KCH1.NE.ICHBLN) RETURN
    3 I1=I1+1
      IF(I1.GE.N1) RETURN
      KCH1=MKCHAR(J1+I1)
      IF(KCH1.EQ.ICHBLN) GO TO 3
      GO TO 2
    4 IF(I1.GE.N1) GO TO 5
      KCH1=MKCHAR(J1+N1)
      IF(KCH1.EQ.ICHBLN) GO TO 5
      I1=I1+1
      GO TO 4
    5 MJFGES=J1+I1
      RETURN
      END
