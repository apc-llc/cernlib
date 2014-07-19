*
* $Id: pagpri.F,v 1.2 1998/12/10 10:49:49 couet Exp $
*
* $Log: pagpri.F,v $
* Revision 1.2  1998/12/10 10:49:49  couet
* - New commands 3DNULL 3DPLINE and 3DPMARKER
*
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/20 19/12/95  10.22.15  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAGPRI
*
*          /GRAPHICS_BASIC/PRIMITIVES
*
#include "hbook/hcbook.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pcbuff.inc"
#include "paw/quest.inc"
#include "paw/pchtag.inc"
*
      CHARACTER*32 CHOPT
      DIMENSION PVALUE(100),IAO(100),IAS(100),IAC(100)
      DIMENSION XX(2),YY(2),UU(3),VV(3)
      EQUIVALENCE (PAWBUF(1601),PVALUE(1)),(PAWBUF(1701),IAO(1))
      EQUIVALENCE (PAWBUF(1801),IAS(1)),(PAWBUF(1901),IAC(1))
*.______________________________________
*
      CALL KUPATL(CHPATL,NPAR)
*
*           AXIS
*
      IF(CHPATL.EQ.'AXIS')THEN
         CALL KUGETR(X0)
         CALL KUGETR(X1)
         CALL KUGETR(Y0)
         CALL KUGETR(Y1)
         CALL KUGETR(WMIN)
         CALL KUGETR(WMAX)
         CALL KUGETI(NDIV)
         CALL KUGETC(CHOPT,NCH)
         CALL IGAXIS(X0,X1,Y0,Y1,WMIN,WMAX,NDIV,CHOPT)
         GO TO 999
      ENDIF
*
*           ARC
*
      IF(CHPATL.EQ.'ARC')THEN
         CALL KUGETR(X1)
         CALL KUGETR(Y1)
         CALL KUGETR(R1)
         CALL KUGETR(R2)
         IF(R2.LT.0.)R2=R1
         CALL KUGETR(PHIMIN)
         CALL KUGETR(PHIMAX)
         IF(PHIMAX.GT.360.)PHIMAX=MOD(PHIMAX,360.)
         CALL IGARC(X1,Y1,R1,R2,PHIMIN,PHIMAX)
         GO TO 999
      ENDIF
*
*           ELLIPSE
*
      IF(CHPATL.EQ.'ELLIPSE')THEN
         CALL KUGETR(XC)
         CALL KUGETR(YC)
         CALL KUGETR(RX)
         CALL KUGETR(RY)
         CALL KUGETR(PHIMIN)
         CALL KUGETR(PHIMAX)
         CALL KUGETR(THETA)
         IF(PHIMAX.GT.360.)PHIMAX=MOD(PHIMAX,360.)
         CALL IGELLI(XC,YC,RX,RY,PHIMIN,PHIMAX,THETA)
         GO TO 999
      ENDIF
*
*           BOX
*
      IF(CHPATL.EQ.'BOX')THEN
         CALL KUGETR(X1)
         CALL KUGETR(X2)
         CALL KUGETR(Y1)
         CALL KUGETR(Y2)
         CALL IGBOX(X1,X2,Y1,Y2)
         GO TO 999
      ENDIF
*
*           DBOX
*
      IF(CHPATL.EQ.'DBOX')THEN
         CALL KUGETR(X1)
         CALL KUGETR(X2)
         CALL KUGETR(Y1)
         CALL KUGETR(Y2)
         CALL HPLTOC(X1,Y1,X1,Y1,NTWIN)
         CALL HPLTOC(X2,Y2,X2,Y2,NTWIN)
         IF(NTWIN.NE.1)CALL ISELNT(1)
         CALL IGBOX(X1,X2,Y1,Y2)
         IF(NTWIN.NE.1)CALL ISELNT(NTWIN)
         GO TO 999
      ENDIF
*
*           FBOX
*
      IF(CHPATL.EQ.'FBOX')THEN
         CALL KUGETR(X1)
         CALL KUGETR(X2)
         CALL KUGETR(Y1)
         CALL KUGETR(Y2)
         CALL KUGETR(X3)
         CALL KUGETR(X4)
         CALL KUGETR(Y3)
         CALL KUGETR(Y4)
         CALL IGFBOX(X1,X2,Y1,Y2,X3,X4,Y3,Y4)
         GO TO 999
      ENDIF
*
*             ARROW
*
      IF(CHPATL.EQ.'ARROW')THEN
         CALL KUGETR(XX(1))
         CALL KUGETR(XX(2))
         CALL KUGETR(YY(1))
         CALL KUGETR(YY(2))
         CALL HPLTOC(XX(1),YY(1),XX(1),YY(1),NTWIN)
         CALL HPLTOC(XX(2),YY(2),XX(2),YY(2),NTWIN)
         IF(NTWIN.NE.1)CALL ISELNT(1)
*
         CALL IPL(2,XX,YY)
*
         CALL KUGETR(SSIZE)
         SIZE=ABS(SSIZE)
         IF(SIZE.LE.0.)GO TO 999
         TGAR=0.6
         FSIN=0.
         FCOS=1.
         XL=SQRT((XX(2)-XX(1))**2 + (YY(2)-YY(1))**2)
   10    IF(XL.GT.0.)THEN
            FSIN=(YY(2)-YY(1))/XL
            FCOS=(XX(2)-XX(1))/XL
         ENDIF
*
         UU(1)=XX(1)+(XL-SIZE)*FCOS-SIZE*TGAR*FSIN
         UU(3)=XX(1)+(XL-SIZE)*FCOS+SIZE*TGAR*FSIN
         VV(1)=YY(1)+(XL-SIZE)*FSIN+SIZE*TGAR*FCOS
         VV(3)=YY(1)+(XL-SIZE)*FSIN-SIZE*TGAR*FCOS
         UU(2)=XX(2)
         VV(2)=YY(2)
         CALL IPL(3,UU,VV)
         IF(SSIZE.LT.0.)THEN
            XX(2)=XX(1)
            XX(1)=UU(2)
            YY(2)=YY(1)
            YY(1)=VV(2)
            SSIZE=1.
            GO TO 10
         ENDIF
         IF(NTWIN.NE.1)CALL ISELNT(NTWIN)
         GO TO 999
      ENDIF
*
*           PLINE
*
      IF(CHPATL.EQ.'PLINE')THEN
         CALL KUGETI(N)
         CALL KUGETV(CHTEMP,LX1,LX2)
         CALL KUGETV(CHTEMP,LY1,LY2)
         IF(LX1.EQ.0)GO TO 999
         IF(LY1.EQ.0)GO TO 999
         NCX=LX2-LX1+1
         NCY=LY2-LY1+1
         IF(N.GT.NCX.OR.N.GT.NCY)THEN
            N=MIN(NCX,NCY)
            CALL HBUG('Number of points exceed vector dimension',
     +                 'PAGPRI',0)
         ENDIF
         CALL IPL(N,Q(LX1),Q(LY1))
         GO TO 999
      ENDIF
*
*           3DPLINE
*
      IF(CHPATL.EQ.'3DPLINE')THEN
         CALL KUGETI(N)
         CALL KUGETV(CHTEMP,LX1,LX2)
         CALL KUGETV(CHTEMP,LY1,LY2)
         CALL KUGETV(CHTEMP,LZ1,LZ2)
         IF(LX1.EQ.0)GO TO 999
         IF(LY1.EQ.0)GO TO 999
         IF(LZ1.EQ.0)GO TO 999
         NCX=LX2-LX1+1
         NCY=LY2-LY1+1
         NCZ=LZ2-LZ1+1
         IF(N.GT.NCX.OR.N.GT.NCY.OR.N.GT.NCZ)THEN
            N=MIN(NCX,NCY)
            N=MIN(N,NCZ)
            CALL HBUG('Number of points exceed vector dimension',
     +                 'PAGPRI',0)
         ENDIF
         CALL IPL3(N,Q(LX1),Q(LY1),Q(LZ1))
         GO TO 999
      ENDIF
*
*           LINE
*
      IF(CHPATL.EQ.'LINE')THEN
         CALL KUGETR(XX(1))
         CALL KUGETR(YY(1))
         CALL KUGETR(XX(2))
         CALL KUGETR(YY(2))
         CALL IPL(2,XX,YY)
         GO TO 999
      ENDIF
*
*           DLINE
*
      IF(CHPATL.EQ.'DLINE')THEN
         CALL KUGETR(XX(1))
         CALL KUGETR(XX(2))
         CALL KUGETR(YY(1))
         CALL KUGETR(YY(2))
         CALL HPLTOC(XX(1),YY(1),XX(1),YY(1),NTWIN)
         CALL HPLTOC(XX(2),YY(2),XX(2),YY(2),NTWIN)
         IF(NTWIN.NE.1)CALL ISELNT(1)
         CALL IPL(2,XX,YY)
         IF(NTWIN.NE.1)CALL ISELNT(NTWIN)
         GO TO 999
      ENDIF
*
*           FAREA
*
      IF(CHPATL.EQ.'FAREA')THEN
         CALL KUGETI(N)
         CALL KUGETV(CHTEMP,LX1,LX2)
         CALL KUGETV(CHTEMP,LY1,LY2)
         IF(LX1.EQ.0)GO TO 999
         IF(LY1.EQ.0)GO TO 999
         NCX=LX2-LX1+1
         NCY=LY2-LY1+1
         IF(N.GT.NCX.OR.N.GT.NCY)THEN
            N=MIN(NCX,NCY)
            CALL HBUG('Number of points exceed vector dimension',
     +                 'PAGPRI',0)
         ENDIF
         CALL IFA(N,Q(LX1),Q(LY1))
         GO TO 999
      ENDIF
*
*           PMARKER
*
      IF(CHPATL.EQ.'PMARKER')THEN
         CALL KUGETI(N)
         CALL KUGETV(CHTEMP,LX1,LX2)
         CALL KUGETV(CHTEMP,LY1,LY2)
         IF(LX1.EQ.0)GO TO 999
         IF(LY1.EQ.0)GO TO 999
         NCX=LX2-LX1+1
         NCY=LY2-LY1+1
         IF(N.GT.NCX.OR.N.GT.NCY)THEN
            N=MIN(NCX,NCY)
            CALL HBUG('Number of points exceed vector dimension',
     +                 'PAGPRI',0)
         ENDIF
         CALL IPM(N,Q(LX1),Q(LY1))
         GO TO 999
      ENDIF
*
*           3DPMARKER
*
      IF(CHPATL.EQ.'3DPMARKER')THEN
         CALL KUGETI(N)
         CALL KUGETV(CHTEMP,LX1,LX2)
         CALL KUGETV(CHTEMP,LY1,LY2)
         CALL KUGETV(CHTEMP,LZ1,LZ2)
         IF(LX1.EQ.0)GO TO 999
         IF(LY1.EQ.0)GO TO 999
         IF(LZ1.EQ.0)GO TO 999
         NCX=LX2-LX1+1
         NCY=LY2-LY1+1
         NCZ=LZ2-LZ1+1
         IF(N.GT.NCX.OR.N.GT.NCY.OR.N.GT.NCZ)THEN
            N=MIN(NCX,NCY)
            N=MIN(N,NCZ)
            CALL HBUG('Number of points exceed vector dimension',
     +                 'PAGPRI',0)
         ENDIF
         CALL IPM3(N,Q(LX1),Q(LY1),Q(LZ1))
         GO TO 999
      ENDIF
*
*             TEXT
*
      IF(CHPATL.EQ.'TEXT')THEN
         CALL KUGETR(X)
         CALL KUGETR(Y)
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETR(SIZE)
         CALL KUGETR(ANGLE)
         CALL KUGETC(CHOPT,NCH)
         CALL HPLTOC(X,Y,XCM,YCM,NTWIN)
         IF(NTWIN.NE.1)CALL ISELNT(1)
         CALL IGTEXT(XCM,YCM,CHTITL,SIZE,ANGLE,CHOPT)
         IF(NTWIN.NE.1)CALL ISELNT(NTWIN)
         GO TO 999
      ENDIF
*
*           ITX
*
      IF(CHPATL.EQ.'ITX')THEN
         CALL KUGETR(X)
         CALL KUGETR(Y)
         CALL KUGETE(CHTITL,NCH)
         CALL HPLTOC(X,Y,XCM,YCM,NTWIN)
         IF(NTWIN.NE.1)CALL ISELNT(1)
         CALL ITX(XCM,YCM,CHTITL(1:NCH))
         IF(NTWIN.NE.1)CALL ISELNT(NTWIN)
         GO TO 999
      ENDIF
*
*           LABELS
*
      IF(CHPATL.EQ.'LABELS')THEN
         CALL KUGETI(LABNUM)
         CALL KUGETI(NLAB)
         CALL KUGETS(CHTAG(1),NCH)
         DO 20 I=1,NLAB
            CALL KUGETL(CHTAG(I),NCH)
   20    CONTINUE
         CALL HPLABL(LABNUM,NLAB,CHTAG)
         CALL IGLBL(NLAB,CHTAG)
         GO TO 999
      ENDIF
*
*           PIE
*
      IF(CHPATL.EQ.'PIE')THEN
         CALL KUGETR(X0)
         CALL KUGETR(Y0)
         CALL KUGETR(RADIUS)
         CALL KUGETI(N)
         CALL KUGETV(CHTEMP,LX1,LX2)
         IF(LX1.EQ.0)GO TO 999
         IF(N.GT.100)N=100
         NCX=LX2-LX1+1
         IF(N.GT.NCX)THEN
            N=NCX
            CALL HBUG('Number of points exceed vector dimension',
     +                 'PAGPRI',0)
         ENDIF
         DO 30  I=1,N
            PVALUE(I)=Q(LX1+I-1)
            IF(PVALUE(I).LE.0.)THEN
               CALL HBUG('Pie cannot have negative or ZERO values',
     +                    'PAGPRI',0)
               GO TO 999
            ENDIF
   30    CONTINUE
         CALL KUGETC(CHOPT,NCHOPT)
         CALL KUGETC(CHTEMP,NCH)
         CALL KUVECT(CHTEMP,LOX1,LOX2)
         IF(LOX1.NE.0)THEN
            CHOPT(NCHOPT+1:NCHOPT+1)='O'
            NCHOPT=NCHOPT+1
            DO 40  I=1,N
               IAO(I)=Q(LOX1+I-1)
   40       CONTINUE
         ENDIF
         CALL KUGETC(CHTEMP,NCH)
         CALL KUVECT(CHTEMP,LSX1,LSX2)
         IF(LSX1.NE.0)THEN
            CHOPT(NCHOPT+1:NCHOPT+1)='S'
            NCHOPT=NCHOPT+1
            DO 50  I=1,N
               IAS(I)=Q(LSX1+I-1)
   50       CONTINUE
         ENDIF
         CALL KUGETC(CHTEMP,NCH)
         CALL KUVECT(CHTEMP,LCX1,LCX2)
         IF(LCX1.NE.0)THEN
            CHOPT(NCHOPT+1:NCHOPT+1)='C'
            NCHOPT=NCHOPT+1
            DO 60  I=1,N
               IAC(I)=Q(LCX1+I-1)
   60       CONTINUE
         ENDIF
         CALL IGPIE(X0,Y0,RADIUS,N,PVALUE,CHOPT,IAO,IAS,IAC)
         GO TO 999
      ENDIF
*
*           GRAPH OR HIST
*
      IF(CHPATL.EQ.'GRAPH'.OR.CHPATL.EQ.'HIST')THEN
         CALL KUGETI(N)
         CALL KUGETV(CHTEMP,LX1,LX2)
         IF(LX1.EQ.0)GO TO 999
         CALL KUGETV(CHTEMP,LY1,LY2)
         IF(LY1.EQ.0)GO TO 999
         LQ(LCDIR-40) = LX1
         LQ(LCDIR-41) = LY1
         NCX          = LX2-LX1+1
         NCY          = LY2-LY1+1
         IF(N.GT.NCX.OR.N.GT.NCY)THEN
            N=MIN(NCX,NCY)
            CALL HBUG('Number of points exceed vector dimension',
     +                 'PAGPRI',0)
         ENDIF
         CALL KUGETC(CHOPT,NCH)
         IW=INDEX(CHOPT,'W')
         CALL PAHLOG(LOGX,LOGY,LOGZ)
         IF(IW.NE.0)THEN
            DO 70  I=1,NCH
               IF(CHOPT(I:I).EQ.'A')CHOPT(I:I)=' '
   70       CONTINUE
            YMIN=VMIN(Q(LY1),N)
            YMAX=VMAX(Q(LY1),N)
            DY=0.05*(YMAX-YMIN)
            IF(DY.EQ.0.)DY=0.05*ABS(YMAX)
            IF(DY.EQ.0.)DY=1.
            IF(CHPATL.EQ.'HIST')THEN
               XMIN=Q(LX1)
               IF(INDEX(CHOPT,'N').NE.0)THEN
                  XMAX=Q(LX1+N)
               ELSE
                  XMAX=Q(LX1+1)
               ENDIF
               DX=0.
            ELSE
               XMIN=VMIN(Q(LX1),N)
               XMAX=VMAX(Q(LX1),N)
               DX=0.05*(XMAX-XMIN)
               IF(DX.EQ.0.)DX=0.05*ABS(XMAX)
               IF(DX.EQ.0.)DX=1.
            ENDIF
            X1=XMIN-DX
            X2=XMAX+DX
            Y1=YMIN-DY
            Y2=YMAX+DY
            IF(X1.LT.0..AND.XMIN.GE.0.)X1=0.
            IF(X2.GT.0..AND.XMAX.LE.0.)X2=0.
            IF(Y1.LT.0..AND.YMIN.GE.0.)Y1=0.
            IF(Y2.GT.0..AND.YMAX.LE.0.)Y2=0.
            IF(LOGX.NE.0)THEN
               X1=0.5*XMIN
               X2=2.*XMAX
            ENDIF
            IF(LOGY.NE.0)THEN
               Y1=0.5*YMIN
               Y2=2.*YMAX
            ENDIF
            CALL HPLFRA(X1,X2,Y1,Y2,' ')
         ENDIF
         CALL UOPTC(CHOPT,'LC*PBFH',IQUEST(81))
         IOPDR = 0
         DO 80 I=1,7
            IOPDR = IOPDR+IQUEST(80+I)
   80    CONTINUE
         IF(IOPDR.NE.0)THEN
            IF(LOGX.NE.0.OR.LOGY.NE.0)CHOPT(32:32) = 'G'
            IF(LOGX.NE.0)CHOPT(31:31)              = 'X'
            IF(LOGY.NE.0)CHOPT(30:30)              = 'Y'
            IF(CHPATL.EQ.'GRAPH')THEN
               IQUEST(81)   = LQ(LCDIR-40)
               IQUEST(82)   = LQ(LCDIR-41)
               CHOPT(29:29) = 'Z'
               CALL IGRAPH(N,Q(LX1),Q(LY1),CHOPT)
            ELSE
               CALL IGHIST(N,Q(LX1),Q(LY1),CHOPT)
            ENDIF
         ENDIF
         CALL PAUTIT(' ')
         GO TO 999
      ENDIF
*
*           PAVE
*
      IF(CHPATL.EQ.'PAVE')THEN
         CALL KUGETR(X1)
         CALL KUGETR(X2)
         CALL KUGETR(Y1)
         CALL KUGETR(Y2)
         CALL KUGETR(DZ)
         CALL KUGETI(ISBOX)
         CALL KUGETI(ISFRAM)
         CALL KUGETC(CHOPT,NCH)
         CALL IGPAVE(X1,X2,Y1,Y2,DZ,ISBOX,ISFRAM,CHOPT)
         GO TO 999
      ENDIF
*
  999 END
