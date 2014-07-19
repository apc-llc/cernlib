*
* $Id: hplwir.F,v 1.2 2001/12/07 15:19:56 couet Exp $
*
* $Log: hplwir.F,v $
* Revision 1.2  2001/12/07 15:19:56  couet
* - use higz include files from the higz include file directory.
*   option Z is now working in hplaer.F
*   option Z has been implemented in hplsym.F
*
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :
      SUBROUTINE HPLWIR(CHOPT,XXVAL,YYVAL,KIMARK)
*.==========>
*.            Routine to draw cross-wires and optionally tick-marks
*.            on the plot
*.            (Transformation NTHIST is selected.)
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl9.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcbook.inc"
#include "higz/hilabs.inc"
#include "hbook/hcflag.inc"
      PARAMETER (LABLEN=32)
      INTEGER CX,CY
      CHARACTER*10 CHAXI,CHAXI1
      CHARACTER*(*) CHOPT,KIMARK
      DIMENSION RTXFP(2)
      DIMENSION IOPT(7)
      EQUIVALENCE (IOPTX ,IOPT(1)),(IOPTY ,IOPT(2))
      EQUIVALENCE (IOPTA ,IOPT(3)),(IOPTB ,IOPT(4))
      EQUIVALENCE (IOPTL ,IOPT(5)),(IOPTR ,IOPT(6))
      EQUIVALENCE (IOPTW ,IOPT(7))
      DIMENSION XAX(2),YAX(2)
      EQUIVALENCE (XAX1,XAX(1)),(XAX2,XAX(2))
      EQUIVALENCE (YAX1,YAX(1)),(YAX2,YAX(2))
      LOGICAL LTICK
*._____________________________
*
*       Prepare the NDIV parameters
*
      NDIVX=INT(RDIVX)
      NDIVY=INT(RDIVY)
      RDIVXA=ABS(RDIVX)
      RDIVYA=ABS(RDIVY)
      IX=100*(RDIVXA+0.005)
      IY=100*(RDIVYA+0.005)
      NX=INT(10*RDIVXA-10*IABS(NDIVX))
      NY=INT(10*RDIVYA-10*IABS(NDIVY))
      CX=IX-100*IABS(NDIVX)-10*NX
      CY=IY-100*IABS(NDIVY)-10*NY
      LW=LABLEN/4
*
*              Optimize the number of divisions as a
*              function of ZONE numbers
*
      IF(IXWIN.GT.1.AND.RDIVX.GT.0)THEN
         NX2=INT(NDIVX/100)
         NX1=MAX(NDIVX-(100*NX2),1)
         NDIVX=(100*NX2)+(NX1/IXWIN)
      ENDIF
      IF(IYWIN.GT.1.AND.RDIVY.GT.0)THEN
         NY2=INT(NDIVY/100)
         NY1=MAX(NDIVY-(100*NY2),1)
         NDIVY=(100*NY2)+(NY1/((IYWIN+1)/2))
      ENDIF
*----
*     Transformation NTHIST is selected
*----
      IF(NTWIN.NE.NTHIST)THEN
         CALL ISELNT(NTHIST)
         NTWIN=NTHIST
      ENDIF
*
      CALL IGQ('TXCI',RTXCI)
      CALL IGQ('TXFP',RTXFP)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('LWID',RLWID)
      CALL ISLN(1)
      CALL ISPLCI(IBCOL)
      CALL ISCLIP(0)
*
      NARG=4
      CALL NOARG(NARG)
      NARG=MIN(NARG,4)
*----
*     Set up defaults
*----
      LTICK=.FALSE.
      XVAL=0.
      YVAL=0.
      IF(NARG.GT.4)NARG=4
      GOTO (40,30,20,10),NARG
*
   10 IF(KIMARK(1:1).EQ.'T')LTICK=.TRUE.
*
   20 YVAL=FLOARG(YYVAL)
*
   30 XVAL=FLOARG(XXVAL)
*
   40 CONTINUE
*----
*     Decode options
*----
      CALL UOPTC(CHOPT,'XYABLRW',IOPT)
      IF(CHOPT.EQ.' ')THEN
         IOPTX=1
         IOPTY=1
      ENDIF
*----
*.      Initialization of axis attributes
*-----
      RTX=(XWMAXI-XWMINI)/(XHIGH-XLOW)
      RTY=(YWMAXI-YWMINI)/(YHIGH-YLOW)
*
*       Set axis label size
*
      ALSIZ=TVSIZ(7)*RTY
      CALL IGSET('LASI',ALSIZ)
*
*              Set axis value font and precision
*
      KFONT  = IABS(IHFONT(3))
      IAXFON = KFONT/10
      IAXPRE = MOD(KFONT,10)
      IF(IHFONT(3).LT.0)IAXFON = -IAXFON
      CALL ISTXFP(IAXFON,IAXPRE)
*
*       Calculate axis ticks mark
*
      YTISIZ=YLTICK*RTX
      XTISIZ=XLTICK*RTY
*
*       Calculate axis label distance
*
      YALDIS=XLVAL*RTX
      XALDIS=YLVAL*RTY
*----
*     Test if cross-wires required perpendicular to x-axis
*----
      IF(IOPTX.EQ.0) GOTO 70
      CALL IGPID(1,'y-axis',ID,' ')
*
      ATEST=(XWMAXI-XWMINI)*0.001
*
      CHAXI='SHD'
      CHAXI1=CHAXI
*
      CALL IGSET('LAOF',YALDIS)
      CALL IGSET('TMSI',YTISIZ)
*
      IF(.NOT.LOGXFL)THEN
         XAX1=XVAL
      ELSE
         IF(XVAL.LE.0.)XVAL=BIGP
         XAX1=LOG10(XVAL)
      ENDIF
*----
*     Test if cross-wires outside range, or if at the edge (where
*     the line is not removed)
*----
      IF(XAX1.GT.XWMAXI)XAX1=XWMAXI
      IF(XAX1.LT.XWMINI) XAX1=XWMINI
      XAX2=XAX1
      YAX1=YWMINI
      YAX2=YWMAXI
      IF(.NOT.LTICK) THEN
         IF((ABS(XAX1-XWMAXI).GE.ATEST) .AND. (ABS(XAX1-XWMINI).GE.
     +   ATEST))THEN
            CALL IPL(2,XAX,YAX)
         ENDIF
         GOTO 70
      ENDIF
      IF(ABS(XAX1-XWMAXI).LT.ATEST)THEN
         CHAXI='+'//CHAXI1
         CHAXI1=CHAXI
         IF(IOPTL.NE.0)THEN
            CHAXI='='//CHAXI1
            CHAXI1=CHAXI
         ENDIF
      ELSE IF(ABS(XAX1-XWMINI).LT.ATEST)THEN
         CHAXI='-'//CHAXI1
         CHAXI1=CHAXI
         IF(IOPTR.NE.0)THEN
            CHAXI='='//CHAXI1
            CHAXI1=CHAXI
         ENDIF
      ELSE
         CHAXI='+-'//CHAXI1
         CHAXI1=CHAXI
         IF(IOPTL.NE.0)THEN
            CHAXI='='//CHAXI1
            CHAXI1=CHAXI
         ENDIF
      ENDIF
*----
*     Plot tick-marks along the cross wire
*----
      IF(.NOT.LOGYFL)THEN
         BMIN=YWMINI
         BMAX=YWMAXI
      ELSE
         BMIN=10**YWMINI
         BMAX=10**YWMAXI
      ENDIF
*
      IF((IOPTR.EQ.0).AND.(IOPTL.EQ.0))THEN
         CHAXI='U'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
      IF(LOGYFL)THEN
         CHAXI='G'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
      IF(IOPTW.NE.0)THEN
         CHAXI='W'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
      IF(LODVYI)THEN
         CHAXI='I'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
      IF(NDIVY.LT.0)THEN
         CHAXI='N'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
*
*   Define the label alignment on the Y Axis. Note that if
*   the labels are on the right, Left and Right text alignments
*   are inverted
*
      IF(CY.EQ.1)THEN
         CHAXI1=CHAXI
         IF (IOPTR.NE.0) THEN
            CHAXI='R'//CHAXI1
         ELSE
            CHAXI='L'//CHAXI1
         ENDIF
      ENDIF
      IF(CY.EQ.2)THEN
         CHAXI1=CHAXI
         CHAXI='C'//CHAXI1
      ENDIF
      IF(CY.EQ.3)THEN
         CHAXI1=CHAXI
         IF (IOPTR.NE.0) THEN
            CHAXI='L'//CHAXI1
         ELSE
            CHAXI='R'//CHAXI1
         ENDIF
      ENDIF
      IF(CY.EQ.4)THEN
         CHAXI1=CHAXI
         IF (IOPTR.NE.0) THEN
            CHAXI='RM'//CHAXI1
         ELSE
            CHAXI='LM'//CHAXI1
         ENDIF
      ENDIF
      IF(CY.EQ.5)THEN
         CHAXI1=CHAXI
         CHAXI='CM'//CHAXI1
      ENDIF
      IF(CY.EQ.6)THEN
         CHAXI1=CHAXI
         IF (IOPTR.NE.0) THEN
            CHAXI='LM'//CHAXI1
         ELSE
            CHAXI='RM'//CHAXI1
         ENDIF
      ENDIF
      IF(CY.EQ.0)THEN
         IF(NY.NE.0)THEN
            CHAXI1=CHAXI
            CHAXI='M'//CHAXI1
         ENDIF
         IF (IOPTR.NE.0) THEN
            CHAXI1=CHAXI
            CHAXI='L'//CHAXI1
         ENDIF
      ENDIF
*
      IF(NY.NE.0)THEN
         LNY = 0
         LN  = LQ(LHPLOT-2)
   50    IF(LN.NE.0)THEN
            IF(IQ(LN+1).EQ.NY)LNY=LN
            LN=LQ(LN)
            GOTO 50
         ENDIF
         DO 60 I=1,IQ(LNY+2)
            HILABS(I)=' '
            CALL UHTOC(IQ(LNY+3+(I-1)*LW),4,HILABS(I),LABLEN)
   60    CONTINUE
         NHILAB=IQ(LNY+2)
         CALL IZLBL
         CHAXI1=CHAXI
         CHAXI='TN'//CHAXI1
      ENDIF
*
      CALL ISTXCI(IYCOL)
      CALL ISLWSC(FLOAT(IYWID))
      CALL IGAXIS(XAX1,XAX2,YAX1,YAX2,BMIN,BMAX,IABS(NDIVY),CHAXI)
*----
*     Test if cross-wires required perpendicular to y-axis
*----
   70 IF(IOPTY.EQ.0) GOTO 999
      CALL IGPID(1,'x-axis',ID,' ')
*
      ATEST=(YWMAXI-YWMINI)*0.001
*
      CHAXI='SHD'
      CHAXI1=CHAXI
*
      CALL IGSET('LAOF',XALDIS)
      CALL IGSET('TMSI',XTISIZ)
*
      IF(.NOT.LOGYFL)THEN
         YAX1=YVAL
      ELSE
         IF(YVAL.LE.0.)YVAL=BIGP
         YAX1=LOG10(YVAL)
      ENDIF
*----
*     Test if cross-wires outside range
*----
      IF(YAX1.GT.YWMAXI)YAX1=YWMAXI
      IF(YAX1.LT.YWMINI) YAX1=YWMINI
      YAX2=YAX1
      XAX1=XWMINI
      XAX2=XWMAXI
      IF(.NOT.LTICK) THEN
         IF((ABS(YAX1-YWMAXI).GE.ATEST) .AND. (ABS(YAX1-YWMINI).GE.
     +   ATEST))THEN
            CALL IPL(2,XAX,YAX)
         ENDIF
         GOTO 999
      ENDIF
      IF(ABS(YAX1-YWMAXI).LT.ATEST)THEN
         CHAXI='-'//CHAXI1
         CHAXI1=CHAXI
         IF(IOPTB.NE.0)THEN
            CHAXI='='//CHAXI1
            CHAXI1=CHAXI
         ENDIF
      ELSE IF(ABS(YAX1-YWMINI).LT.ATEST)THEN
         CHAXI='+'//CHAXI1
         CHAXI1=CHAXI
         IF(IOPTA.NE.0)THEN
            CHAXI='='//CHAXI1
            CHAXI1=CHAXI
         ENDIF
      ELSE
         CHAXI='+-'//CHAXI1
         CHAXI1=CHAXI
         IF(IOPTA.NE.0)THEN
            CHAXI='='//CHAXI1
            CHAXI1=CHAXI
         ENDIF
      ENDIF
*----
*     Plot tick marks along the cross-wire
*----
      IF(.NOT.LOGXFL)THEN
         BMIN=XWMINI
         BMAX=XWMAXI
      ELSE
         BMIN=10**XWMINI
         BMAX=10**XWMAXI
      ENDIF
*
      IF((IOPTA.EQ.0).AND.(IOPTB.EQ.0))THEN
         CHAXI='U'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
      IF(LOGXFL)THEN
         CHAXI='G'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
      IF(IOPTW.NE.0)THEN
         CHAXI='W'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
      IF(LODVXI)THEN
         CHAXI='I'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
      IF(NDIVX.LT.0)THEN
         CHAXI='N'//CHAXI1
         CHAXI1=CHAXI
      ENDIF
*
      IF(CX.EQ.1)THEN
         CHAXI1=CHAXI
         CHAXI='C'//CHAXI1
      ENDIF
      IF(CX.EQ.2)THEN
         CHAXI1=CHAXI
         CHAXI='YC'//CHAXI1
      ENDIF
      IF(CX.EQ.3)THEN
         CHAXI1=CHAXI
         CHAXI='LO'//CHAXI1
      ENDIF
      IF(CX.EQ.4)THEN
         CHAXI1=CHAXI
         CHAXI='R0'//CHAXI1
      ENDIF
      IF(CX.EQ.5)THEN
         CHAXI1=CHAXI
         CHAXI='MC'//CHAXI1
      ENDIF
      IF(CX.EQ.6)THEN
         CHAXI1=CHAXI
         CHAXI='MCY'//CHAXI1
      ENDIF
      IF(CX.EQ.7)THEN
         CHAXI1=CHAXI
         CHAXI='MLO'//CHAXI1
      ENDIF
      IF(CX.EQ.8)THEN
         CHAXI1=CHAXI
         CHAXI='MR0'//CHAXI1
      ENDIF
      IF(CX.EQ.0)THEN
         IF(NX.NE.0)THEN
            CHAXI1=CHAXI
            CHAXI='MC'//CHAXI1
         ELSE
            CHAXI1=CHAXI
            CHAXI='C'//CHAXI1
         ENDIF
      ENDIF
      IF(NX.NE.0)THEN
         LNX = 0
         LN  = LQ(LHPLOT-2)
   80    IF(LN.NE.0)THEN
            IF(IQ(LN+1).EQ.NX)LNX=LN
            LN=LQ(LN)
            GOTO 80
         ENDIF
         DO 90 I=1,IQ(LNX+2)
            HILABS(I)=' '
            CALL UHTOC(IQ(LNX+3+(I-1)*LW),4,HILABS(I),LABLEN)
   90    CONTINUE
         NHILAB=IQ(LNX+2)
         CALL IZLBL
         CHAXI1=CHAXI
         CHAXI='TN'//CHAXI1
      ENDIF
*
      CALL ISTXCI(IXCOL)
      CALL ISLWSC(FLOAT(IXWID))
      CALL IGAXIS(XAX1,XAX2,YAX1,YAX2,BMIN,BMAX,IABS(NDIVX),CHAXI)
*
  999 CALL ISCLIP(1)
      CALL ISLN(ILTYP)
      CALL IGSET('TXCI',RTXCI)
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('LWID',RLWID)
*
      END
