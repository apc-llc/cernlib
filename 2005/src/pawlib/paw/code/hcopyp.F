*
* $Id: hcopyp.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hcopyp.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/00 25/10/93  12.38.01  by  Rene Brun
*-- Author :    Rene Brun   04/04/91
      SUBROUTINE HCOPYP(ID2,CHTITL)
*.==========>
*.           To copy a projection of ID (in LFIX) into a 1-Dim ID2
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      CHARACTER*(*) CHTITL
*.___________________________________________
*
      NCX=IQ(LPRX)
      XMIN=Q(LPRX+1)
      XMAX=Q(LPRX+2)
      LR1=LCONT
      IF(LR1.EQ.0)GO TO 99
      IF(LPRY.NE.0)GO TO 99
      CALL HBOOK1(ID2,CHTITL,NCX,XMIN,XMAX,0.)
      IF(LCID.EQ.0)GO TO 99
      CALL MZDROP(IHDIV,LQ(LCID-1),' ')
      CALL MZCOPY(IHDIV,LR1,IHDIV,LCID,-1,' ')
      LCONT=LQ(LCID-1)
      IF(LCONT.EQ.0)GO TO 99
      NTOT=IQ(LCID-1)+IQ(LCONT-1)+20
      IF(LQ(LCONT).NE.0)THEN
         LW=LQ(LCONT)
         NTOT=NTOT+IQ(LW-1)+10
      ENDIF
      IQ(LCID+KNTOT)=NTOT
*
  99  END

