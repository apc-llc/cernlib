*
* $Id: hplsta.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplsta.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/03 04/09/95  12.11.53  by  O.Couet
*-- Author :
      SUBROUTINE HPLSTA(ID,KICASE,NUM)
*.==========>
*.           Print statistics informations on picture.
*.           (Transformation no 1 is selected)
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl13.inc"
#include "hbook/hcbits.inc"
      LOGICAL HEXIST
      DIMENSION W(9),XLINE(2),YLINE(2),VPNT(4),VP1(4)
      DIMENSION IBIT(7),NCHARD(2)
      DIMENSION RTXFP(2), RTXAL(2)
      CHARACTER*(*) KICASE
      CHARACTER*30 CTEXT
      CHARACTER*8 CHTEXT(7)
      SAVE ICOLER
      DATA ICOLER /0/
      DATA CHTEXT /'ID','Entries','Mean','RMS','UDFLW','OVFLW'
     +,            'ALLCHAN'/
*._____________________________
*
      IF(.NOT.HEXIST(ID))RETURN
      IF(I123.EQ.0)RETURN
      NCHARD(1)=20
      NCHARD(2)=20
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('TXCI',RTXCI)
      CALL IGQ('TXFP',RTXFP)
      CALL IGQ('CHHE',RCHHE)
      CALL IGQ('TANG',RTANG)
      CALL IGQ('TXAL',RTXAL)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('BORD',RBORD)
      CALL IGQ('LTYP',RLTYP)
      CALL IGQ('LWID',RLWID)
*
*             Set transformation no 1 (cm)
*
      NTWOLD=NTWIN
      CALL ISELNT(NTHIST)
      CALL IGQWK(1,'NTVP',VPNT)
      CALL ISELNT(1)
      CALL IGQWK(1,'NTVP',VP1)
      NTWIN=1
*
*             Set defaults and get common information
*
      CALL HNOENT(ID,NOENT)
      CALL HGSTAT(ID,W)
      IF(W(4).LT.-1.0E10)GOTO 20
*
*             2-D Histogram
*
      NLINES=5
      NLINES=0
      NOPTIO=3
      ISTAT2=ISTAT
      DO 10 I=NOPTIO,1,-1
         IBIT(I)=ISTAT2/(10**(I-1))
         IF(IBIT(I).NE.0)THEN
            NLINES=NLINES+1
            IF(I.EQ.3)THEN
               NLINES=NLINES+2
               NCHARD(2)=30
            ENDIF
         ENDIF
         ISTAT2=ISTAT2-(IBIT(I)*(10**(I-1)))
   10 CONTINUE
      IDIM=2
      GOTO 50
*
   20 CONTINUE
*
*             1-D Histogram
*
      CALL IGPID(1,'stat-box',ID,' ')
      IFIT2=IFIT
      IF(IFIT.GE.100)IFIT2=IFIT-100
      IF(LOFIT.AND.IFIT2-10.GT.0)NCHARD(1)=30
      NLINES=0
      NOPTIO=7
      ISTAT2=ISTAT
      DO 30 I=NOPTIO,1,-1
         IBIT(I)=ISTAT2/(10**(I-1))
         IF(IBIT(I).NE.0)NLINES=NLINES+1
         ISTAT2=ISTAT2-(IBIT(I)*(10**(I-1)))
   30 CONTINUE
      IDIM=1
      AMEAN=HSTATI(ID,1,KICASE,NUM)
      STDEV=HSTATI(ID,2,KICASE,NUM)
      ALLCHA=0.
      DO 40 I=1,NCX
         ALLCHA=ALLCHA+HCX(I,1)
   40 CONTINUE
*
   50 CONTINUE
      CBSIZ=TVSIZ(5)/IXWIN
      BOXHEI=CBSIZ*FLOAT(NLINES)*2.
      BOXLEN=CBSIZ*FLOAT(NCHARD(IDIM))
*
      XL    = XSIZ*((VPNT(1)-VP1(1))/(VP1(2)-VP1(1)))
      YL    = YSIZ*((VPNT(3)-VP1(3))/(VP1(4)-VP1(3)))
      XH    = XSIZ*((VPNT(2)-VP1(1))/(VP1(2)-VP1(1)))
      YH    = YSIZ*((VPNT(4)-VP1(3))/(VP1(4)-VP1(3)))
      XBOXL = XH-BOXLEN
      YBOXL = YH-BOXHEI
      XBOXH = XH
      YBOXH = YH
      XOFF  = (XH-XL)*SMRIT
      YOFF  = (YH-YL)*SMGUP
*
*             Define starting position for text
*
      X1 = XBOXL+CBSIZ-XOFF
      XC = (XBOXH-XBOXL)/2.0+XBOXL-XOFF
      X2 = XBOXH-CBSIZ-XOFF
      Y  = YBOXH+0.5*CBSIZ-YOFF
      Y2 = Y
*
*             Draw the STAT box
*
      CALL ISLN(ILTYP)
      CALL ISLN(1)
      CALL ISPLCI(IXCOL)
      CALL ISTXCI(IXCOL)
      CALL ISLWSC(1.)
      SSIZ=0.
      IF(IOPTU.NE.0)THEN
         I=INT(IBTYP/1000)-1
         IF(I.EQ.1)THEN
            ICOLER=IBCOL
         ELSE
            ICOLER=0
         ENDIF
         CALL ISFAIS(1)
         CALL IGSET('BORD',1.)
         CALL ISFACI(ICOLER)
         CALL IGBOX(XBOXL-XOFF,XBOXH-XOFF,YBOXL-YOFF,YBOXH-YOFF)
      ELSE
         CTEXT='TRS'
         SSIZ=CBSIZ
         IF(ISSCOL.EQ.0)SSIZ=0.
         CALL IGPAVE(XBOXL-SSIZ-XOFF,XBOXH-SSIZ-XOFF
     +,              YBOXL-SSIZ-YOFF,YBOXH-SSIZ-YOFF
     +,              SSIZ,0,1000+ISSCOL,CTEXT)
         Y  = Y-SSIZ
         Y2 = Y2-SSIZ
         X2 = X2-SSIZ
         X1 = X1
         XC = XC-SSIZ
      ENDIF
*
*             Set text size, font and precision
*
      KFONT=IABS(IHFONT(5))
      ISTFON=KFONT/10
      ISTPRE=MOD(KFONT,10)
      IF(IHFONT(5).LT.0)ISTFON=-ISTFON
      CALL ISTXFP(ISTFON,ISTPRE)
      CALL ISCHH(CBSIZ)
      CALL IGSET('TANG',0.)
*
      IF (IDIM.EQ.2)GOTO 70
*
*             Draw the comments for 1D histograms
*
      CALL ISTXAL(0,0)
      DO 60 I=1,NOPTIO
         IF(IBIT(I).NE.0)THEN
            Y2=Y2-2.0*CBSIZ
            CALL ITX(X1,Y2,CHTEXT(I))
         ENDIF
   60 CONTINUE
*
*             STAT for 1-D histo
*
      CALL ISTXAL(3,0)
      IF(NCHARD(1).EQ.30)THEN
         XX=XC+2*CBSIZ
      ELSE
         XX=X2
      ENDIF
*
*             - 1) ID
*
      IF(IBIT(1).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10000) ID
         CALL ITX(XX,Y,CTEXT)
      ENDIF
*
*             - 2) Number of entries
*
      IF(IBIT(2).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10000) NOENT
         CALL ITX(XX,Y,CTEXT)
      ENDIF
*
*             - 3) Mean value
*
      IF(IBIT(3).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10100) AMEAN
         CALL ITX(XX,Y,CTEXT)
      ENDIF
*
*             - 4) Standard deviation
*
      IF(IBIT(4).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10100) STDEV
         CALL ITX(XX,Y,CTEXT)
      ENDIF
*
*             - 5) Number of underflow
*
      IF(IBIT(5).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10100) W(1)
         CALL ITX(XX,Y,CTEXT)
      ENDIF
*
*             - 6) Number of overflow
*
      IF(IBIT(6).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10100) W(3)
         CALL ITX(XX,Y,CTEXT)
      ENDIF
*
*             - 7) All channels
*
      IF(IBIT(7).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10100) ALLCHA
         CALL ITX(XX,Y,CTEXT)
      ENDIF
      GOTO 80
*
*
*             STAT for 2-D histo
*
*             - 1) ID
*
   70 IF(IBIT(1).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CALL ISTXAL(0,0)
         CALL ITX(X1,Y,'ID')
         CTEXT=' '
         WRITE(CTEXT,10000) ID
         CALL ISTXAL(3,0)
         CALL ITX(X2,Y,CTEXT)
      ENDIF
*
*
*             - 2) Number of entries
*
      IF(IBIT(2).NE.0)THEN
         Y=Y-2.0*CBSIZ
         CALL ISTXAL(0,0)
         CALL ITX(X1,Y,'ENTRIES')
         CTEXT=' '
         WRITE(CTEXT,10000) NOENT
         CALL ISTXAL(3,0)
         CALL ITX(X2,Y,CTEXT)
      ENDIF
*
*             - 3) Draw grid for STAT
*
*             - 3.1) Horizontal lines
*
      IF(IBIT(3).NE.0)THEN
         YBACK=Y
         Y=Y-0.5*CBSIZ
         XLINE(1)=XBOXL-SSIZ-XOFF
         XLINE(2)=XBOXH-SSIZ-XOFF
         YLINE(1)=Y
         YLINE(2)=Y
         CALL IPL(2,XLINE,YLINE)
         Y=Y-2.0*CBSIZ
         YLINE(1)=Y
         YLINE(2)=Y
         CALL IPL(2,XLINE,YLINE)
         Y=Y-2.0*CBSIZ
         YLINE(1)=Y
         YLINE(2)=Y
         CALL IPL(2,XLINE,YLINE)
*
*             - 3.2) Vertical lines
*
         YLINE(2)=YBOXL-SSIZ-YOFF
         YLINE(1)=YBACK-0.5*CBSIZ
         XLINE(1)=(XBOXH-XBOXL)/3.+XBOXL-XOFF
         XLINE(2)=XLINE(1)
         CALL IPL(2,XLINE,YLINE)
         XLINE(1)=(XBOXH-XBOXL)*2./3.+XBOXL-XOFF
         XLINE(2)=XLINE(1)
         CALL IPL(2,XLINE,YLINE)
         Y=YBACK
*
*             - 4) Output 1st STAT line
*
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10200) W(1)
         CALL ISTXAL(0,0)
         CALL ITX(X1,Y,CTEXT)
         CTEXT=' '
         WRITE(CTEXT,10200) W(2)
         CALL ISTXAL(2,0)
         CALL ITX(XC,Y,CTEXT)
         CTEXT=' '
         WRITE(CTEXT,10200) W(3)
         CALL ISTXAL(3,0)
         CALL ITX(X2,Y,CTEXT)
*
*             - 5) Output 2nd STAT line
*
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10200) W(4)
         CALL ISTXAL(0,0)
         CALL ITX(X1,Y,CTEXT)
         CTEXT=' '
         WRITE(CTEXT,10200) W(5)
         CALL ISTXAL(2,0)
         CALL ITX(XC,Y,CTEXT)
         CTEXT=' '
         WRITE(CTEXT,10200) W(6)
         CALL ISTXAL(3,0)
         CALL ITX(X2,Y,CTEXT)
*
*             - 6) Output 3rd STAT line
*
         Y=Y-2.0*CBSIZ
         CTEXT=' '
         WRITE(CTEXT,10200) W(7)
         CALL ISTXAL(0,0)
         CALL ITX(X1,Y,CTEXT)
         CTEXT=' '
         WRITE(CTEXT,10200) W(8)
         CALL ISTXAL(2,0)
         CALL ITX(XC,Y,CTEXT)
         CTEXT=' '
         WRITE(CTEXT,10200) W(9)
         CALL ISTXAL(3,0)
         CALL ITX(X2,Y,CTEXT)
      ENDIF
*
*               Reset NT
*
   80 IF(NTWIN.NE.NTWOLD)THEN
         NTWIN=NTWOLD
         CALL ISELNT(NTWOLD)
      ENDIF
      IF(LOZFL)CALL IZPICT(' ','O')
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('TXCI',RTXCI)
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      CALL IGSET('CHHE',RCHHE)
      CALL IGSET('TANG',RTANG)
      CALL ISTXAL(INT(RTXAL(1)),INT(RTXAL(2)))
      CALL IGSET('FACI',RFACI)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('BORD',RBORD)
      CALL IGSET('LTYP',RLTYP)
      CALL IGSET('LWID',RLWID)
*
10000 FORMAT(I15)
10100 FORMAT(G11.4)
10200 FORMAT(G10.3)
*
      END
