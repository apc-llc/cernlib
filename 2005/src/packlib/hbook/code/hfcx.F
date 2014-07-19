*
* $Id: hfcx.F,v 1.2 2000/12/19 16:56:22 couet Exp $
*
* $Log: hfcx.F,v $
* Revision 1.2  2000/12/19 16:56:22  couet
* - Bug fixed in RMS calculation. When IDOPT STAT is on the RMS calculation
*   was wrong one some machines because it was done using REAL and DOUBLE
*   PRECISION numbers. Now we use on ly DOUBLE PRECISION numbers.
*
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/03 24/05/94  12.55.16  by  Rene Brun
*-- Author :
      SUBROUTINE HFCX(ICX,W)
*.==========>
*.        Fills channel number ICX with W
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
#if !defined(CERNLIB_DOUBLE)
      DIMENSION SWX(4)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SWX(2),DBLX,DBLZ
#endif
*.___________________________________________
      NB=IQ(LCONT+KNBIT)
      IF(NB.GE.32)THEN
         Q(LCONT+KCON1+ICX)=W
      ELSE
         NBB=32/NB
         L1=ICX/NBB
         L2=(ICX-NBB*L1)*NB +1
         INC=W
         IF(INC.LT.0)INC=0
         IF(INC.GT.MAXBIT(NB))INC=MAXBIT(NB)
         CALL SBYT(INC,IQ(LCONT+L1+KCON1),L2,NB)
      ENDIF
      AW=ABS(W)
      IF(W.GT.1.E6.OR.W.LT.0..OR.MOD(AW,1.).GT.1.E-5)THEN
         IQ(LCONT+KNOENT)=-1
      ELSE
         IF(IQ(LCONT+KNOENT).GE.0)THEN
            IF(IQ(LCONT+KNOENT).LT.10000000)THEN
               IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+INT(W)
            ELSE
               IQ(LCONT+KNOENT)=-1
            ENDIF
         ENDIF
      ENDIF
*
      IF(I7.NE.0)THEN
         IF(ICX.EQ.0)THEN
            CALL VZERO(Q(LCONT+KSTAT1),6)
         ELSEIF(ICX.LE.IQ(LPRX))THEN
            X=Q(LPRX+1)+(FLOAT(ICX-1)+0.5)*(Q(LPRX+2)-Q(LPRX+1))/
     +        FLOAT(IQ(LPRX))
            Z=ABS(W)
            Q(LCONT+KSTAT1)=Q(LCONT+KSTAT1)+Z
            Q(LCONT+KSTAT1+1)=Q(LCONT+KSTAT1+1)+Z*Z
            CALL UCOPY(Q(LCONT+KSTAT1+2),SWX,4)
#if defined(CERNLIB_DOUBLE)
            DBLX=X   
            DBLZ=Z
            SWX(1)=SWX(1)+DBLZ*DBLX
            SWX(2)=SWX(2)+DBLZ*DBLX*DBLX
#else
            SWX(1)=SWX(1)+Z*X
            SWX(2)=SWX(2)+Z*X*X
#endif
            CALL UCOPY(SWX,Q(LCONT+KSTAT1+2),4)
         ENDIF
      ENDIF
      END
