*
* $Id: hfpak1.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfpak1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 10/09/93  09.23.36  by  Rene Brun
*-- Author :
      SUBROUTINE HFPAK1(ID1,NID,X,N)
*.==========>
*.           FAST FILLING ROUTINE FOR 1-DIM HIST
*.           NID IS AN OUTPUT PARAMETER = HIST NUMBER-1
*.           N IS THE NUMBER OF ELEMENTS OF ARRAY X TO BE FILLED
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
      DIMENSION X(1),NID(1)
      SAVE BWID1
*.___________________________________________
#include "hbook/jbyt.inc"
      IF(ID1.NE.IDLAST)THEN
*
*             CHECK IF ID1 IS AT THE ADDRESS CORRESPONDING TO NID(1)
*             IF NOT COMPUTE NID(1).
*
         IF(NID(1).LE.0)GO TO 10
         IF(NID(1).GT.IQ(LCDIR+KNRH))GO TO 10
         LCID=LQ(LTAB-NID(1))
         IF(IQ(LTAB+NID(1)).EQ.ID1)GO TO 20
*
*             SEARCH ADDRESS OF ID IN THE ADDRESS TABLE (BINARY SEARCH)
*
  10     ID=ID1
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         NID(1)=IDPOS
*
*             COMPUTE POINTERS TO ACCESS HISTOGRAM AREA
*
  20     IDLAST=ID1
         LCID =LQ(LTAB-NID(1))
         LCONT=LQ(LCID-1)
         BWID1=FLOAT(IQ(LCID+KNCX))/(Q(LCID+KXMAX)-Q(LCID+KXMIN))
      ENDIF
*
*             INCREMENT NUMBER OF ENTRIES AND COMPUTE CHANNEL NUMBER
*
      IQ(LCONT+KNOENT)=IQ(LCONT+KNOENT)+N
      DO 30 I=1,N
         IF(X(I).LT.Q(LCID+KXMIN))THEN
            ICHAN=0
         ELSEIF(.NOT.(X(I).LT.Q(LCID+KXMAX)))THEN
            ICHAN=IQ(LCID+KNCX)+1
         ELSE
            ICHAN=(X(I)-Q(LCID+KXMIN))*BWID1+1
         ENDIF
         IF(IQ(LCONT+KNBIT).GE.32)THEN
            Q(LCONT+ICHAN+KCON1)=Q(LCONT+ICHAN+KCON1)+1.
         ELSE
            NBPROX=IQ(LCONT+KNBIT)
            NB=32/NBPROX
            LWORD=ICHAN/NB
            LBIT=(ICHAN-NB*LWORD)*NBPROX+1
            INC=JBYT(IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)+1
            IF(INC.GT.MAXBIT(NBPROX))INC=MAXBIT(NBPROX)
            CALL SBYT(INC,IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
         ENDIF
  30  CONTINUE
      END
