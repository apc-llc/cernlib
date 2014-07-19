*
* $Id: hkff1.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hkff1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 14/02/94  17.27.25  by  Rene Brun
*-- Author :    Rene Brun   14/02/94
      SUBROUTINE HKFF1(LINE,ID1,X,W)
*.==========>
*.           Special fast filling routine for COMIS.
*.           Fast calls to HF1 are replaced by this routine.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcfast.inc"
      SAVE BWID1
*.___________________________________________
      IF(ID1.NE.IDLAST)THEN
*
*             CHECK IF ID1 IS AT THE ADDRESS CORRESPONDING TO NID(LINE)
*             IF NOT COMPUTE NID(LINE).
*
         IF(NID(LINE).LE.0)GO TO 10
         IF(NID(LINE).GT.IQ(LCDIR+KNRH))GO TO 10
         LCID=LQ(LTAB-NID(LINE))
         IF(IQ(LTAB+NID(LINE)).EQ.ID1)GO TO 20
*
*             SEARCH ADDRESS OF ID IN THE ADDRESS TABLE (BINARY SEARCH)
*
  10     ID=ID1
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         NID(LINE)=IDPOS
*
*             COMPUTE POINTERS TO ACCESS HISTOGRAM AREA
*
  20     IDLAST=ID1
         LCID =LQ(LTAB-NID(LINE))
         LCONT=LQ(LCID-1)
         BWID1=FLOAT(IQ(LCID+KNCX))/(Q(LCID+KXMAX)-Q(LCID+KXMIN))
      ENDIF
*
*             INCREMENT NUMBER OF ENTRIES AND COMPUTE CHANNEL NUMBER
*
      IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+1
      IF(X.LT.Q(LCID+KXMIN))THEN
         ICHAN=0
      ELSEIF(.NOT.(X.LT.Q(LCID+KXMAX)))THEN
         ICHAN=IQ(LCID+KNCX)+1
      ELSE
         ICHAN=(X-Q(LCID+KXMIN))*BWID1+1
      ENDIF
      IF(IQ(LCONT+KNBIT).GE.32)THEN
         Q(LCONT+ICHAN+KCON1)=Q(LCONT+ICHAN+KCON1)+W
      ELSE
         NBPROX=IQ(LCONT+KNBIT)
         NB=32/NBPROX
         LWORD=ICHAN/NB
         LBIT=(ICHAN-NB*LWORD)*NBPROX+1
         INC=JBYT(IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
         INC=INC+W+0.5
         IF(INC.GT.MAXBIT(NBPROX))INC=MAXBIT(NBPROX)
         CALL SBYT(INC,IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
      ENDIF
      END
