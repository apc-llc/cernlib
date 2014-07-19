*
* $Id: hfilpf.F,v 1.7 2000/12/19 16:56:22 couet Exp $
*
* $Log: hfilpf.F,v $
* Revision 1.7  2000/12/19 16:56:22  couet
* - Bug fixed in RMS calculation. When IDOPT STAT is on the RMS calculation
*   was wrong one some machines because it was done using REAL and DOUBLE
*   PRECISION numbers. Now we use on ly DOUBLE PRECISION numbers.
*
* Revision 1.6  1998/10/05 14:39:01  couet
* - New option P for profile histograms. Implemented by:  Nello Nappi
*   <Nello.Nappi@cern.ch>
*
* Revision 1.5  1996/10/08 16:21:17  couet
* - rounding problem fixed
*
* Revision 1.4  1996/09/20 08:54:26  couet
* - IFIX come back ... But now at the right place ...
*
* Revision 1.3  1996/09/18 15:34:19  couet
* - The previous fixe in this routine (IFIX ...) had catastrophic side
*   effects.
*
* Revision 1.2  1996/05/24 12:25:26  couet
* - Rounding problem. IFIX added
*
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
      SUBROUTINE HFILPF(ID1,X,Y,W)
*.==========>
*.           FAST FILLING ENTRY FOR A PROFILE HISTOGRAM
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#if !defined(CERNLIB_DOUBLE)
      DIMENSION SWX(4)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SWX(2),DBLX,DBLZ
#endif
*.___________________________________________
*
      IF(ID1.NE.IDLAST)THEN
         ID=ID1
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         IDLAST=ID1
         LCID=LQ(LTAB-IDPOS)
         LCONT=LQ(LCID-1)
         LPRX=LCID+KNCX
         I7=JBIT(IQ(LCID+KBITS),7)
      ENDIF
*
      IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+1
      IF(Y.LT.Q(LCID+KMIN1))RETURN
      IF(Y.GT.Q(LCID+KMAX1))RETURN
      Z=ABS(W)
      IF(X.LT.Q(LPRX+1))THEN
C
C underflow
C
         ICHAN=0
      ELSEIF(X.GE.Q(LPRX+2))THEN
C
C overflow
C
         ICHAN=IQ(LPRX)+1
      ELSE
C
C In range
C
         ICHAN=IFIX(
     +              (X-Q(LPRX+1))*
     +              FLOAT(IQ(LPRX))/(Q(LPRX+2)-Q(LPRX+1))
     +             ) + 1
         IF (ICHAN.EQ.IQ(LPRX)+1) GOTO 10
         IF (ICHAN.EQ.0)          GOTO 10
         LW=LQ(LCONT)
         LN=LQ(LW)
         IF(JBIT(IQ(LW),3).NE.0)THEN
            IF(Q(LN+ICHAN).NE.0.)THEN
               YDIF=Y-Q(LCONT+ICHAN+KCON1)/Q(LN+ICHAN)
               Q(LW+ICHAN)=Q(LW+ICHAN)+
     +                     Z*Q(LN+ICHAN)*YDIF*YDIF/(Z+Q(LN+ICHAN))
            ENDIF
         ELSE
            Q(LW+ICHAN)=Q(LW+ICHAN)+Z*Y*Y
         ENDIF
         Q(LN+ICHAN)=Q(LN+ICHAN)+W
*
* I7 means HBSTAT has been called
*
         IF(I7.NE.0)THEN
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
*
 10   Q(LCONT+ICHAN+KCON1)=Q(LCONT+ICHAN+KCON1)+Z*Y
*
      END
