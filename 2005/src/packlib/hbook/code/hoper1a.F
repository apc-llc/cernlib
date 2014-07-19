*
* $Id: hoper1a.F,v 1.1.1.1 1996/01/16 17:07:44 mclareni Exp $
*
* $Log: hoper1a.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/01 28/10/93  11.24.26  by  Rene Brun
*-- Author :    Rene Brun   28/10/93
      SUBROUTINE HOPER1A(NCX,ID1,ID2,C1)
*.==========>
*.           Auxiliary of HOPERA for 1-D histograms with alphanumeric labels
*.==========> ( R.Brun )
      CHARACTER*16 CHLAB
*.___________________________________________
*
*        Loop on all channels on ID1 and fill ID2
*
      DO 10 I=1,NCX
         CALL HLGNXT(ID1,I,CHLAB,'X')
         W=C1*HI(ID1,I)
         CALL HLPOS(ID2,CHLAB,IPOS,'X')
         IPOS=MAX(IPOS,0)
         CALL HFC1(ID2,IPOS,CHLAB,W,'X')
  10  CONTINUE
      END
