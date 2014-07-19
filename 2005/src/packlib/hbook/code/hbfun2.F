*
* $Id: hbfun2.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbfun2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/02 21/05/93  16.58.46  by  Rene Brun
*-- Author :
      SUBROUTINE HBFUN2(IDD,CHTITL,NCHAX,XMIN,XMAX,NCHAY,YMIN,YMAX,FUNC)
*.==========>
*.           creation of a 2-Dim-plot and filling with FUNC
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
      CHARACTER*(*) CHTITL
      EXTERNAL FUNC
*.___________________________________________
      NCHX=INTARG(NCHAX)
      NCHY=INTARG(NCHAY)
      XMI=FLOARG(XMIN)
      XMA=FLOARG(XMAX)
      YMI=FLOARG(YMIN)
      YMA=FLOARG(YMAX)
*
      CALL HBOOK2(IDD,CHTITL,NCHX,XMI,XMA,NCHY,YMI,YMA,0.)
*
      CALL HFIND(IDD,'HBFUN2')
      IF(LCID.EQ.0)GO TO 99
*
      LCONT=LQ(LCID-1)
      DX=(XMA-XMI)/FLOAT(NCHX)
      DY=(YMA-YMI)/FLOAT(NCHY)
      X=XMI-0.5*DX
*
      DO 20 I=1,NCHX
         X=X+DX
         Y=YMI-0.5*DY
         DO 10 J=1,NCHY
            Y=Y+DY
            XX=X
            YY=Y
            W=FUNC(XX,YY)
            CALL HF2(IDD,X,Y,W)
  10     CONTINUE
  20  CONTINUE
      IDLAST=0
*
  99  RETURN
      END
