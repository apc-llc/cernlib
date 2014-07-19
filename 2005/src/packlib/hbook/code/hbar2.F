*
* $Id: hbar2.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: hbar2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.39.07  by  Julian Bunn
*-- Author :    Rene Brun   20/06/94
      SUBROUTINE HBAR2(IDD)
*.==========>
*.           create bank to store sum of square of weigths
*.           for 2-D histograms
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      IRET = 3
   10 CALL HLOOP (IDD,'HBAR2 ',IRET)
      IF (IRET .EQ. 0)                 GO TO 999
      CALL HDCOFL
      IF(I230.EQ.0)GO TO 100
      NCX=IQ(LCID+KNCX)
      NCY=IQ(LCID+KNCY)
*
*            2-DIM histogram
*
      LCONT=LQ(LCID-1)
      LSCAT=LCONT
      LW=LQ(LCONT)
      IF(LW.NE.0)GO TO 100
      NTOT=NCX*NCY+10
      CALL HSPACE(NTOT,'HBAR2 ',IDD)
      IF(IERR.NE.0)GO TO 100
      CALL MZBOOK(IHDIV,LW,LCONT,0,'HI2E',0,0,NCX*NCY,3,0)
      CALL SBIT0(IQ(LCID),5)
      NB=IQ(LCONT+KNBIT)
      K=0
      DO 30 I=1,NCY
         DO 20 J=1,NCX
            K=K+1
            XX=HCXY(J,I,1)
            Q(LW+K)=ABS(XX)
  20     CONTINUE
  30  CONTINUE
*
  80  IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
*
  100 IRET  = 2
      GO TO 10
 999  RETURN
      END
