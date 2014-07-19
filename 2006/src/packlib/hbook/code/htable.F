*
* $Id: htable.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: htable.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.54  by  Rene Brun
*-- Author :
      SUBROUTINE HTABLE(ID2,CHTITL,NNX,XX0,XX1,NNY,YY0,YY1,VALMAX)
*.==========>
*.           BOOKING OF A TABLE
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
      CHARACTER*(*) CHTITL
*.___________________________________________
      NV=1
      XVALMA=1000.
      IFW=9
      CALL NOARG(IFW)
      IF(IFW.EQ.9)XVALMA=FLOARG(VALMAX)
      CALL HBOOK2(ID2,CHTITL,NNX,XX0,XX1,NNY,YY0,YY1,XVALMA)
      NV=2
      END
