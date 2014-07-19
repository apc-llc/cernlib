*
* $Id: hid2.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hid2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.49  by  Rene Brun
*-- Author :
      SUBROUTINE HID2(IDVECT,N)
*.==========>
*.           RETURN LIST OF ALL 2-DIM IDS
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      DIMENSION IDVECT(1)
*.___________________________________________
#include "hbook/jbyt.inc"
      N=0
      NRHIST=IQ(LCDIR+KNRH)
      DO 10 I=1,NRHIST
         LC=LQ(LTAB-I)
         IF(JBYT(IQ(LC+KBITS),2,2).EQ.0)GO TO 10
         N=N+1
         IDVECT(N)=IQ(LTAB+I)
  10  CONTINUE
  99  RETURN
      END
