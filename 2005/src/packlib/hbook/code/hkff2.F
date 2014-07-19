*
* $Id: hkff2.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hkff2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 15/02/94  09.17.01  by  Rene Brun
*-- Author :    Rene Brun   14/02/94
      SUBROUTINE HKFF2(LINE,ID1,X,Y,W)
*.==========>
*.           Special fast filling routine for COMIS.
*.           Fast calls to HF2 are replaced by this routine.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcfast.inc"
      SAVE NBSCAT,BWIDX1,BWIDY1
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
         LCID  =LQ(LTAB-NID(LINE))
         LSCAT =LQ(LCID-1)
         NBSCAT=IQ(LSCAT+KNBIT)
         BWIDX1=FLOAT(IQ(LCID+KNCX))/(Q(LCID+KXMAX)-Q(LCID+KXMIN))
         BWIDY1=FLOAT(IQ(LCID+KNCY))/(Q(LCID+KYMAX)-Q(LCID+KYMIN))
      ENDIF
*
*             INCREMENT NUMBER OF ENTRIES AND COMPUTE CELL NUMBER
*
      IQ(LSCAT+KNOENT)=IQ(LSCAT+KNOENT)+1
      IF(X.LT.Q(LCID+KXMIN))THEN
         I=0
      ELSEIF(.NOT.(X.LT.Q(LCID+KXMAX)))THEN
         I=IQ(LCID+KNCX)+1
      ELSE
         I=(X-Q(LCID+KXMIN))*BWIDX1 + 1
      ENDIF
      IF(Y.LT.Q(LCID+KYMIN))THEN
         J=0
      ELSEIF(.NOT.(Y.LT.Q(LCID+KYMAX)))THEN
         J=IQ(LCID+KNCY)+1
      ELSE
         J=(Y-Q(LCID+KYMIN))*BWIDY1 + 1
      ENDIF
*
      ICHAN=(IQ(LCID+KNCY)-J+1)*(IQ(LCID+KNCX)+2)+I
      IF(NBSCAT.LT.32)THEN
         NB=32/NBSCAT
         LWORD=ICHAN/NB
         LBIT=(NB-1-MOD(ICHAN,NB))*NBSCAT + 1
         IAD=LWORD+LSCAT+KCON2
         INC=JBYT(IQ(IAD),LBIT,NBSCAT)+W+0.5
         IF(INC.GT.MAXBIT(NBSCAT))INC=MAXBIT(NBSCAT)
         CALL SBYT(INC,IQ(IAD),LBIT,NBSCAT)
      ELSE
         LWORD=LSCAT+ICHAN+KCON2
         Q(LWORD)=Q(LWORD)+W
      ENDIF
      END
