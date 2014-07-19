*
* $Id: pahvec.F,v 1.2 1998/04/09 12:39:09 couet Exp $
*
* $Log: pahvec.F,v $
* Revision 1.2  1998/04/09 12:39:09  couet
* -     The H/GET/ABS gaves a wrong value for histograms with one bin only.
*       The following macro shows the problem:
*
*        h/cre/1d 100 'essai' 1 0.9 1.1
*        v/cre x(1)
*        h/get/abs 100 x
*        v/pri x
*
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/16 03/08/94  10.44.54  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAHVEC
*
*           /HISTOGRAM/GET_VEC
*           /HISTOGRAM/PUT_VEC
*
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "paw/pawcom.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
      CHARACTER*32 CHPATH(5),CHPAT2
*
      CALL KUPATH(CHPATH,NLEV,NPAR)
      CHPATL=CHPATH(NLEV)
      CHPAT2=CHPATH(NLEV-1)
*
*
      IF(CHPAT2.EQ.'PUT_VECT'.AND.CHPATL.EQ.'CONTENTS')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETV(CHTITL,LLOW,LHIGH)
         IF(LLOW.EQ.0)GO TO 99
         CALL HGIVE(ID,CHTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,NWT,IDB)
         IF(NCX.LE.0)GO TO 99
         NMAX=LHIGH-LLOW+1
         IF(NCY.LE.0)THEN
            IF(NCX.GT.NMAX)GO TO 90
         ELSE
            IF(NCX*NCY.GT.NMAX)GO TO 90
         ENDIF
         CALL HPAK(ID,Q(LLOW))
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPAT2.EQ.'PUT_VECT'.AND.CHPATL.EQ.'ERRORS')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETV(CHTITL,LLOW,LHIGH)
         IF(LLOW.EQ.0)GO TO 99
         CALL HGIVE(ID,CHTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,NWT,IDB)
         IF(NCX.LE.0)GO TO 99
         NMAX=LHIGH-LLOW+1
         IF(NCX.GT.NMAX)GO TO 90
         CALL HPAKE(ID,Q(LLOW))
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPAT2.EQ.'GET_VECT'.AND.CHPATL.EQ.'CONTENTS')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETV(CHTITL,LLOW,LHIGH)
         IF(LLOW.EQ.0)GO TO 99
         CALL HGIVE(ID,CHTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,NWT,IDB)
         IF(NCX.LE.0)GO TO 99
         NMAX=LHIGH-LLOW+1
         IF(LFIX.EQ.0)THEN
            IF(NCY.LE.0)THEN
               IF(NCX.GT.NMAX)GO TO 90
            ELSE
               IF(NCX*NCY.GT.NMAX)GO TO 90
            ENDIF
         ELSE
            IF(IQ(LPRX).GT.NMAX)GO TO 90
         ENDIF
         CALL HUNPAK(ID,Q(LLOW),'HIST',1)
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPAT2.EQ.'GET_VECT'.AND.CHPATL.EQ.'ERRORS')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETV(CHTITL,LLOW,LHIGH)
         IF(LLOW.EQ.0)GO TO 99
         CALL HGIVE(ID,CHTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,NWT,IDB)
         IF(NCX.LE.0)GO TO 99
         NMAX=LHIGH-LLOW+1
         IF(NCX.GT.NMAX)GO TO 90
         CALL HUNPKE(ID,Q(LLOW),'HIST',1)
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPAT2.EQ.'GET_VECT'.AND.CHPATL.EQ.'FUNCTION')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETV(CHTITL,LLOW,LHIGH)
         IF(LLOW.EQ.0)GO TO 99
         CALL HGIVE(ID,CHTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,NWT,IDB)
         IF(NCX.LE.0)GO TO 99
         NMAX=LHIGH-LLOW+1
         DO 20 I=1,MIN(NCX,NMAX)
            Q(LLOW+I-1)=HIF(ID,I)
  20     CONTINUE
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPAT2.EQ.'GET_VECT'.AND.CHPATL.EQ.'ABSCISSA')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETV(CHTITL,LLOW,LHIGH)
         IF(LLOW.EQ.0)GO TO 99
         CALL HGIVE(ID,CHTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,NWT,IDB)
         IF(NCX.LE.0)GO TO 99
         NMAX=LHIGH-LLOW+1
         NC=MIN(NCX,NMAX)
         IF (NC.LE.1) THEN
            Q(LLOW)=(XMAX+XMIN)/2.
         ELSE
            DO 30 I=1,NC
               CALL HIX(ID,I,Q(LLOW+I-1))
  30        CONTINUE
            DO 40 I=1,NC-1
               DX=(Q(LLOW+I)-Q(LLOW+I-1))/2.
               Q(LLOW+I-1)=Q(LLOW+I-1)+DX
  40        CONTINUE
            IF(I6.EQ.0)THEN
               Q(LLOW+NC-1)=Q(LLOW+NC-1)+DX
            ELSE
               CALL HIX(ID,NC+1,DX)
               Q(LLOW+NC-1)=0.5*(Q(LLOW+NC-1)+DX)
            ENDIF
         ENDIF
         CALL HSETCD
         GO TO 99
      ENDIF
C
C             REBIN
C
      IF(CHPATL.EQ.'REBIN')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 99
         CALL KUGETV(CHTITL,LX1,LX2)
         CALL KUGETV(CHTITL,LY1,LY2)
         CALL KUGETV(CHTITL,LEX1,LEX2)
         CALL KUGETV(CHTITL,LEY1,LEY2)
         IF(LX1.EQ.0.OR.LY1.EQ.0.OR.LEX1.EQ.0.OR.LEY1.EQ.0)THEN
            CALL HSETCD
            GO TO 99
         ENDIF
         CALL KUGETI(N)
         CALL KUGETI(IFIRST)
         CALL KUGETI(ILAST)
         CALL KUGETC(CHPAT2,NCH)
         IF(INDEX(CHPAT2,'N').NE.0)IFIRST=-IABS(IFIRST)
         CALL HGIVE(ID,CHTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,NWT,IDB)
         IF(NCX.LE.0)GO TO 99
         IF(LX2-LX1+1.LT.N)GO TO 90
         IF(LY2-LY1+1.LT.N)GO TO 90
         IF(LEX2-LEX1+1.LT.N)GO TO 90
         IF(LEY2-LEY1+1.LT.N)GO TO 90
         CALL HREBIN(ID,Q(LX1),Q(LY1),Q(LEX1),Q(LEY1),N,IFIRST,ILAST)
         CALL HSETCD
         GO TO 99
      ENDIF
*
*             Error
*
  90  CALL HBUG('Vector size too small','PAHVEC',ID)
*
  99  END
