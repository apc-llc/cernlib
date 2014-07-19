*
* $Id: pahbok.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahbok.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/02 24/05/93  10.18.03  by  Alfred Nathaniel
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAHBOK
*
*            /HISTOGRAM/BOOK
*                All booking options
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
      CHARACTER*8  CHOPT
*
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'1DHISTO')THEN
         CALL PAGETI(ID)
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETI(NCX)
         CALL KUGETR(XLOW)
         CALL KUGETR(XUP)
         CALL KUGETR(VALMAX)
         CALL HBOOK1(ID,CHTITL,NCX,XLOW,XUP,VALMAX)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'PROFILE')THEN
         CALL PAGETI(ID)
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETI(NCX)
         CALL KUGETR(XLOW)
         CALL KUGETR(XUP)
         CALL KUGETR(YMIN)
         CALL KUGETR(YMAX)
         CALL KUGETC(CHOPT,NCH)
         CALL HBPROF(ID,CHTITL,NCX,XLOW,XUP,YMIN,YMAX,CHOPT)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'BINS')THEN
         CALL PAGETI(ID)
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETI(NCX)
         CALL KUGETV(CHTEMP,LX1,LX2)
         CALL KUGETR(VALMAX)
         IF(LX1.NE.0)THEN
            IF(LX2-LX1+1.LT.NCX) THEN
               CALL HBUG('Vector XBINS too short','PAHBOK',ID)
            ELSE
               CALL HBOOKB(ID,CHTITL,NCX,Q(LX1),VALMAX)
            ENDIF
         ENDIF
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'2DHISTO')THEN
         CALL PAGETI(ID)
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETI(NCX)
         CALL KUGETR(XLOW)
         CALL KUGETR(XUP)
         CALL KUGETI(NCY)
         CALL KUGETR(YLOW)
         CALL KUGETR(YUP)
         CALL KUGETR(VALMAX)
         CALL HBOOK2(ID,CHTITL,NCX,XLOW,XUP,NCY,YLOW,YUP,VALMAX)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'PROX')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL HBPROX(ID,0.)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'PROY')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL HBPROY(ID,0.)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'SLIX')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETI(N)
         CALL HBSLIX(ID,N,0.)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'SLIY')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETI(N)
         CALL HBSLIY(ID,N,0.)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'BANX')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETR(YMIN)
         CALL KUGETR(YMAX)
         CALL HBANDX(ID,YMIN,YMAX,0.)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'BANY')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETR(XMIN)
         CALL KUGETR(XMAX)
         CALL HBANDY(ID,XMIN,XMAX,0.)
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'TITLE_GLOBAL')THEN
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETC(CHOPT,NCH)
         CALL CLTOU(CHOPT)
         IF(INDEX(CHOPT,'U').NE.0)THEN
            CHUTIT=CHTITL
         ELSE
            CALL HTITLE(CHTITL)
         ENDIF
         GO TO 99
      ENDIF
*
  99  END
