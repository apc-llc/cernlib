*
* $Id: hbfun1.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbfun1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 02/04/93  08.38.48  by  Rene Brun
*-- Author :
      SUBROUTINE HBFUN1(IDD,CHTITL,NCHA,XMIN,XMAX,FUNC)
*.==========>
*.           creation of a 1-dim id and filling with FUNC
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
      CHARACTER*(*) CHTITL
      EXTERNAL FUNC
*.___________________________________________
      NCH=INTARG(NCHA)
      XMI=FLOARG(XMIN)
      XMA=FLOARG(XMAX)
*
      CALL HBOOK1(IDD,CHTITL,NCH,XMI,XMA,0.)
*
      CALL HFIND(IDD,'HBFUN1')
      IF(LCID.EQ.0)GO TO 99
      CALL HIDOPT(ID,'STAR')
*
      LFUNC=LQ(LCID-1)
      DX=(XMA-XMI)/FLOAT(NCH)
      X=XMI-0.5*DX
*
      DO 10 I=1,NCH
         X=X+DX
         XX=X
         Q(LFUNC+KCON1+I)=FUNC(XX)
  10  CONTINUE
      IQ(LFUNC+KNOENT)=NCH
*
  99  RETURN
      END
