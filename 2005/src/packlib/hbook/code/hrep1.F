*
* $Id: hrep1.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hrep1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 01/09/89  08.59.21  by  Rene Brun
*-- Author :
      SUBROUTINE HREP1(IDD,ICX,VALUE)
*.==========>
*.           Replace contents of channel ICX by new value VALUE
*.           (only for 1-DIM histograms)
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      CALL HFIND(IDD,'HREP1')
      IF(LCID.LE.0)GO TO 99
      CALL HDCOFL
      IF(I1.EQ.0)GO TO 99
      CALL SBIT0(IQ(LCID),5)
      IF(NB.GE.32)THEN
         Q(LCONT+KCON1+ICX)=VALUE
      ELSE
         NBB=32/NB
         L1=ICX/NBB
         L2=(ICX-NBB*L1)*NB +1
         INC=VALUE
         IF(INC.LT.0)INC=0
         IF(INC.GT.MAXBIT(NB))INC=MAXBIT(NB)
         CALL SBYT(INC,IQ(LCONT+L1+KCON1),L2,NB)
      ENDIF
*
  99  RETURN
      END
