*
* $Id: hfixid.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hfixid.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/10 24/06/94  09.52.21  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE HFIXID(IDD,CHCASE,NUM)
*.==========>
*.           Fix addresses LCONT,etc for a given projection
*.           ICASE,NUM.
*..=========>
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcbits.inc"
      CHARACTER*(*) CHCASE
*.___________________________________________
      CALL HFIND(IDD,'HFIXID')
      IF(LCID.EQ.0)RETURN
      CALL HDCOFL
      IF(I123.EQ.0)GO TO 90
      LFIX=1
      LPRY=0
      CALL UCTOH(CHCASE,ICASE,4,4)
      JCASE=IUCOMP(ICASE,IDENT,8)
*
*            Simple histogram
*
      IF(JCASE.LT.3)THEN
         LCONT=LQ(LCID-1)
         IF(I1.NE.0)THEN
            LPRX=LCID+KNCX
            GO TO 99
         ENDIF
         LPRY=LCID+KNCY
         LSCAT=LCONT
         GO TO 99
      ENDIF
      IF(I230.EQ.0)GO TO 90
*
*             Projection X
*
      IF(JCASE.EQ.3)THEN
         LCONT=LQ(LCID-2)
         LPRX=LCID+KNCX
         IF(LCONT.EQ.0)GO TO 90
         GO TO 99
      ENDIF
*
*             Projection Y
*
      IF(JCASE.EQ.4)THEN
         LCONT=LQ(LCID-3)
         LPRX=LCID+KNCY
         IF(LCONT.EQ.0)GO TO 90
         GO TO 99
      ENDIF
*
*             Slices X
*
      IF(JCASE.EQ.5)THEN
         LSLIX=LQ(LCID-4)
         LPRX=LCID+KNCX
         IF(LSLIX.NE.0)THEN
            IF(NUM.GT.IQ(LSLIX-2))GO TO 90
            LCONT=LQ(LSLIX-NUM)
            GO TO 99
         ELSE
            GO TO 90
         ENDIF
      ENDIF
*
*             Slices Y
*
      IF(JCASE.EQ.6)THEN
         LSLIY=LQ(LCID-5)
         LPRX=LCID+KNCY
         IF(LSLIY.NE.0)THEN
            IF(NUM.GT.IQ(LSLIY-2))GO TO 90
            LCONT=LQ(LSLIY-NUM)
            GO TO 99
         ELSE
            GO TO 90
         ENDIF
      ENDIF
*
*              Bande X
*
      IF(JCASE.EQ.7)THEN
         LBANX=LQ(LCID-6)
         LPRX=LCID+KNCX
         IF(LBANX.NE.0)THEN
            NBX=1
  40        LCONT=LQ(LBANX-1)
            IF(NUM.EQ.NBX)GO TO 99
            NBX=NBX+1
            LBANX=LQ(LBANX)
            IF(LBANX.NE.0)GO TO 40
            GO TO 90
         ENDIF
      ENDIF
*
*             Bande Y
*
      IF(JCASE.EQ.8)THEN
         LBANY=LQ(LCID-7)
         LPRX=LCID+KNCY
         IF(LBANY.NE.0)THEN
            NBY=1
  50        LCONT=LQ(LBANY-1)
            IF(NUM.EQ.NBY)GO TO 99
            NBY=NBY+1
            LBANY=LQ(LBANY)
            IF(LBANY.NE.0)GO TO 50
            GO TO 90
         ENDIF
      ENDIF
*
*           Error Option ICASE,NUM not found
*
  90  LFIX=0
*
  99  IF(LFIX.NE.0)NB=IQ(LCONT+KNBIT)
      END
