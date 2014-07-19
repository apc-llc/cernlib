*
* $Id: hsifla.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hsifla.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 31/08/89  17.46.57  by  Rene Brun
*-- Author :
      SUBROUTINE HSIFLA(KFLA,I)
*.==========>
*.           SETS BIT NO KFLA TO I
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
      DIMENSION IFLAG(32)
      EQUIVALENCE       (IFLAG(1),I1)
*.___________________________________________
      IF(I.NE.0)THEN
         CALL SBIT1(IQ(LCID+KBITS),KFLA)
      ELSE
         CALL SBIT0(IQ(LCID+KBITS),KFLA)
      ENDIF
*
*          Status word bit 5 means that histogram
*          in memory has been modified (for PAW use only)
*
      CALL SBIT0(IQ(LCID),5)
*
      IFLAG(KFLA)=I
      I230=I2+I3
      I123=I1+I230
      END
