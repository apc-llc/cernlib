*
* $Id: hid1.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hid1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.49  by  Rene Brun
*-- Author :
      SUBROUTINE HID1(IDVECT,N)
*.==========>
*.           RETURN LIST OF ALL 1-DIM IDS
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      DIMENSION IDVECT(1)
*.___________________________________________
      N=0
      NRHIST=IQ(LCDIR+KNRH)
      DO 10 I=1,NRHIST
         LC=LQ(LTAB-I)
         IF(JBIT(IQ(LC+KBITS),1).EQ.0)GO TO 10
         N=N+1
         IDVECT(N)=IQ(LTAB+I)
  10  CONTINUE
      END
