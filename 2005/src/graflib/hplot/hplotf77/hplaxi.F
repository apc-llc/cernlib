*
* $Id: hplaxi.F,v 1.2 2001/12/07 15:19:56 couet Exp $
*
* $Log: hplaxi.F,v $
* Revision 1.2  2001/12/07 15:19:56  couet
* - use higz include files from the higz include file directory.
*   option Z is now working in hplaer.F
*   option Z has been implemented in hplsym.F
*
* Revision 1.1.1.1  1996/01/19 10:49:57  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/01 05/05/95  16.42.14  by  O.Couet
*-- Author :
      SUBROUTINE HPLAXI(XX,YY,IOPT)
*.==========>
*.
*.    Draws axis Ox and Oy using histogram coordinates
*. (including ticks marks and labels) XX(3),YY(3)
*. extremities of Ox and Oy axis (histogram coordinates)
*. (Transformation histogram NTHIST must be selected
*. before calling HPLAXI)
*.
*. IOPT = 0 ID is unvalid in this routine
*. IOPT = 1 ID is valid in this routine
*. IOPT = 2 Redraw the X-Axis without labels. This is useful
*           when the HCOL is greater tha 1000.
*.
*.==========>
#include "hbook/hcbook.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl9.inc"
#include "higz/hilabs.inc"
#include "hbook/hcflag.inc"
      COMMON /HPLYZO/ Y1ZOOM,Y2ZOOM
      LOGICAL HLABEQ
      INTEGER HLABNB
      PARAMETER (LABLEN=32)
      CHARACTER*20 CHAXI
      CHARACTER*16 CHAX0
      DIMENSION XX(3),YY(3)
      DIMENSION RTXFP(2)
      INTEGER CX,CY
      SAVE XTMSIZ,YTMSIZ
      DATA XTMSIZ /0./
      DATA YTMSIZ /0./
*._____________________________
*
*              Prepare the NDIV parameters
*
      NDIVX  = INT(RDIVX)
      NDIVY  = INT(RDIVY)
      RDIVXA = ABS(RDIVX)
      RDIVYA = ABS(RDIVY)
      IX     = 100*(RDIVXA+0.005)
      IY     = 100*(RDIVYA+0.005)
      NX     = INT(10*RDIVXA-10*IABS(NDIVX))
      NY     = INT(10*RDIVYA-10*IABS(NDIVY))
      CX     = IX-100*IABS(NDIVX)-10*NX
      CY     = IY-100*IABS(NDIVY)-10*NY
      LW     = LABLEN/4
      IGXY   = 0
*
*              Optimize the number of divisions as a
*              function of ZONE numbers
*
      IF(IXWIN.GT.1.AND.RDIVX.GT.0)THEN
         NX2   = INT(NDIVX/100)
         NX1   = MAX(NDIVX-(100*NX2),1)
         NDIVX = (100*NX2)+(NX1/IXWIN)
      ENDIF
      IF(IYWIN.GT.1.AND.RDIVY.GT.0)THEN
         NY2   = INT(NDIVY/100)
         NY1   = MAX(NDIVY-(100*NY2),1)
         NDIVY = (100*NY2)+(NY1/((IYWIN+1)/2))
      ENDIF
*
*              Transformation rapports between
*              histogram coordinates and cm
*
      IF(XHIGH.GT.XLOW)THEN
         RTX = (XMAXI-XMINI)/(XHIGH-XLOW)
      ELSE
         RETURN
      ENDIF
      IF(YHIGH.GT.YLOW)THEN
         RTY = (YMAXI-YMINI)/(YHIGH-YLOW)
      ELSE
         RETURN
      ENDIF
*
      CALL IGQ('TXFP',RTXFP)
      CALL IGQ('TXCI',RTXCI)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('LWID',RLWID)
      CALL ISCLIP(0)
      IF(IBBCOL.NE.0)THEN
         CALL ISPLCI(IBBCOL)
      ELSE
         CALL ISPLCI(IBCOL)
      ENDIF
*
*              Set axis value size
*
      ALSIZ = TVSIZ(7)*RTY
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
*              Calculate axis ticks mark
*
      YTISIZ = YLTICK*RTX
      XTISIZ = XLTICK*RTY
      XYTSIZ = ABS(XTISIZ)*1.5
*
*              Calculate axis label offset
*
      YALDIS = ABS(XLVAL)*RTX
      XALDIS = ABS(YLVAL)*RTY
*.______________________________________
*
*              Oy axis
*
      IF(IOPT.NE.0)THEN
         CALL IGPID(1,'y-axis',ID,' ')
      ELSE
         CALL IGPID(1,'y-axis',0,' ')
      ENDIF
      CALL ISTXCI(IYCOL)
      CALL ISLWSC(FLOAT(IYWID))
      IF(.NOT.LOGYFL)THEN
         IF(IOPT.EQ.2)THEN
            BMIN = YWMINI
            BMAX = YWMAXI
         ELSE
            BMIN = YMINI
            BMAX = YMAXI
         ENDIF
      ELSE
         IF(IOPT.EQ.2)THEN
            BMIN = 10**YWMINI
            BMAX = 10**YWMAXI
         ELSE
            BMIN = 10**YMINI
            BMAX = 10**YMAXI
         ENDIF
      ENDIF
*
      CHAXI='SDH'
      CHAX0=CHAXI
      CALL IGSET('LAOF',YALDIS)
      IF(YTISIZ.LT.0.)THEN
         YTISIZ   = -YTISIZ
         IF(XLVAL.GE.0.)THEN
            CHAXI = '+='//CHAX0
         ELSE
            CHAXI = '+'//CHAX0
         ENDIF
      ELSE
         IF(XLVAL.LT.0.)CHAXI='-='//CHAX0
      ENDIF
      IF(IOPT.EQ.2)THEN
         CHAX0=CHAXI
         CHAXI='U'//CHAX0
         CALL IGSET('TMSI',YTMSIZ)
      ELSE
         CALL IGSET('TMSI',YTISIZ)
         YTMSIZ = YTISIZ
      ENDIF
      IF(LOGYFL)THEN
         CHAX0 = CHAXI
         CHAXI = 'G'//CHAX0
      ENDIF
      IF(LODVYI)THEN
         CHAX0 = CHAXI
         CHAXI = 'I'//CHAX0
      ENDIF
      IF(NDIVY.LT.0)THEN
         CHAX0 = CHAXI
         CHAXI = 'N'//CHAX0
      ENDIF
      IF(LOGRID)THEN
         CHAX0 = CHAXI
         IGXY  = INT(IGTYP/1000)
         IF(IGXY.NE.2)THEN
            CHAXI = 'W'//CHAX0
            CALL IGSET('AWLN',XX(3))
         ENDIF
      ENDIF
      IF(CY.EQ.1)THEN
         CHAX0 = CHAXI
         CHAXI = 'L'//CHAX0
      ENDIF
      IF(CY.EQ.2)THEN
         CHAX0 = CHAXI
         CHAXI = 'C'//CHAX0
      ENDIF
      IF(CY.EQ.3)THEN
         CHAX0 = CHAXI
         CHAXI = 'R'//CHAX0
      ENDIF
      IF(CY.EQ.4)THEN
         CHAX0 = CHAXI
         CHAXI = 'LM'//CHAX0
      ENDIF
      IF(CY.EQ.5)THEN
         CHAX0 = CHAXI
         CHAXI = 'CM'//CHAX0
      ENDIF
      IF(CY.EQ.6)THEN
         CHAX0 = CHAXI
         CHAXI = 'RM'//CHAX0
      ENDIF
      IF(CY.EQ.0)THEN
         IF(NY.NE.0)THEN
            CHAX0 = CHAXI
            CHAXI = 'RM'//CHAX0
         ELSE
            CHAX0 = CHAXI
            CHAXI = 'R'//CHAX0
         ENDIF
      ENDIF
      IF(NY.NE.0)THEN
         LNY = 0
         LN  = LQ(LHPLOT-2)
         IF(LN.EQ.0)GOTO 30
   10    IF(LN.NE.0)THEN
            IF(IQ(LN+1).EQ.NY)LNY = LN
            LN = LQ(LN)
            GOTO 10
         ENDIF
         NHILAB = MIN(IQ(LNY+2),MAXLAB)
         DO 20 I=1,NHILAB
            HILABS(I) = ' '
            CALL UHTOC(IQ(LNY+3+(I-1)*LW),4,HILABS(I),LABLEN)
   20    CONTINUE
         CALL IZLBL
         CHAX0 = CHAXI
         CHAXI = 'TN'//CHAX0
      ENDIF
*
   30 IAXDIV = IABS(NDIVY)
*
* Oy axis is alphanumeric
*
      IF(IOPT.NE.0)THEN
         IF(HLABEQ(ID,'Y'))THEN
            IF(LOZOOM)THEN
               IAXDIV = Y2ZOOM-Y1ZOOM+1
               ILAB1 = Y1ZOOM
               ILAB2 = Y2ZOOM
               IF(IAXDIV.GT.99)THEN
                  IAXDIV = 99
                  ILAB2 = ILAB1+MAXLAB-1
               ENDIF
            ELSE
               IAXDIV = MIN(HLABNB(ID,'Y'),99)
               ILAB1 = 1
               ILAB2 = IAXDIV
            ENDIF
            NHILAB = IAXDIV
            DO 40 I=ILAB1,ILAB2
               CALL HLGNXT(ID,I,HILABS(I-ILAB1+1),'Y')
   40       CONTINUE
            CALL IZLBL
            CHAX0 = CHAXI
            CHAXI = 'TNRM'//CHAX0
         ENDIF
      ENDIF
*
      NCHX = LENOCC(CHAXI)
      ILT  = MAX(IGTYP-1000*IGXY,1)
      CALL ISLN(ILT)
      CALL IGAXIS(XX(1),XX(2),YY(1),YY(2),BMIN,BMAX,IAXDIV,
     +            CHAXI(1:NCHX))
*.______________________________________
*
*              Ox axis
*
      IF(IOPT.NE.0)THEN
         CALL IGPID(1,'x-axis',ID,' ')
      ELSE
         CALL IGPID(1,'x-axis',0,' ')
      ENDIF
      CALL ISTXCI(IXCOL)
      CALL ISLWSC(FLOAT(IXWID))
      IF(.NOT.LOGXFL)THEN
         IF(IOPT.EQ.2)THEN
            BMIN=XWMINI
            BMAX=XWMAXI
         ELSE
            BMIN=XMINI
            BMAX=XMAXI
         ENDIF
      ELSE
         IF(IOPT.EQ.2)THEN
            BMIN=10**XWMINI
            BMAX=10**XWMAXI
         ELSE
            BMIN=10**XMINI
            BMAX=10**XMAXI
         ENDIF
      ENDIF
*
      CHAXI='SDH'
      CHAX0=CHAXI
      CALL IGSET('LAOF',XALDIS)
      IF(XTISIZ.LT.0.)THEN
         XTISIZ=-XTISIZ
         IF(YLVAL.GE.0.)THEN
            CHAXI='-='//CHAX0
         ELSE
            CHAXI='-'//CHAX0
         ENDIF
      ELSE
         IF(YLVAL.LT.0.)CHAXI='+='//CHAX0
      ENDIF
      IF(IOPT.EQ.2)THEN
         CHAX0=CHAXI
         CHAXI='U'//CHAX0
         CALL IGSET('TMSI',XTMSIZ)
      ELSE
         CALL IGSET('TMSI',XTISIZ)
         XTMSIZ = XTISIZ
      ENDIF
      IF(LOGXFL)THEN
         CHAX0=CHAXI
         CHAXI='G'//CHAX0
      ENDIF
      IF(LODVXI)THEN
         CHAX0=CHAXI
         CHAXI='I'//CHAX0
      ENDIF
      IF(NDIVX.LT.0)THEN
         CHAX0=CHAXI
         CHAXI='N'//CHAX0
      ENDIF
      IF(LOGRID)THEN
         CHAX0=CHAXI
         IGXY=INT(IGTYP/1000)
         IF(IGXY.NE.1)THEN
            CHAXI='W'//CHAX0
            CALL IGSET('AWLN',YY(2))
         ENDIF
      ENDIF
      IF(CX.EQ.1)THEN
         CHAX0=CHAXI
         CHAXI='C'//CHAX0
      ENDIF
      IF(CX.EQ.2)THEN
         CHAX0=CHAXI
         CHAXI='YC'//CHAX0
      ENDIF
      IF(CX.EQ.3)THEN
         CHAX0=CHAXI
         CHAXI='LO'//CHAX0
      ENDIF
      IF(CX.EQ.4)THEN
         CHAX0=CHAXI
         CHAXI='R0'//CHAX0
      ENDIF
      IF(CX.EQ.5)THEN
         CHAX0=CHAXI
         CHAXI='MC'//CHAX0
      ENDIF
      IF(CX.EQ.6)THEN
         CHAX0=CHAXI
         CHAXI='MCY'//CHAX0
      ENDIF
      IF(CX.EQ.7)THEN
         CHAX0=CHAXI
         CHAXI='MLO'//CHAX0
      ENDIF
      IF(CX.EQ.8)THEN
         CHAX0=CHAXI
         CHAXI='MR0'//CHAX0
      ENDIF
      IF(CX.EQ.0)THEN
         IF(NX.NE.0)THEN
            CHAX0=CHAXI
            CHAXI='MC'//CHAX0
         ELSE
            CHAX0=CHAXI
            CHAXI='C'//CHAX0
         ENDIF
      ENDIF
      IF(NX.NE.0)THEN
         LNX = 0
         LN  = LQ(LHPLOT-2)
         IF(LN.EQ.0)GOTO 90
   60    IF(LN.NE.0)THEN
            IF(IQ(LN+1).EQ.NX)LNX=LN
            LN=LQ(LN)
            GOTO 60
         ENDIF
         NHILAB = MIN(IQ(LNX+2),MAXLAB)
         DO 70 I=1,NHILAB
            HILABS(I)=' '
            CALL UHTOC(IQ(LNX+3+(I-1)*LW),4,HILABS(I),LABLEN)
   70    CONTINUE
         CALL IZLBL
         CHAX0=CHAXI
         CHAXI='TN'//CHAX0
      ENDIF
*
      IAXDIV=IABS(NDIVX)
*
* Ox axis is alphanumeric
*
      IF(IOPT.NE.0)THEN
         IF(HLABEQ(ID,'X'))THEN
            IF(LOZOOM)THEN
               IAXDIV = NCMAX-NCMIN+1
               ILAB1 = NCMIN
               ILAB2 = NCMAX
               IF(IAXDIV.GT.99)THEN
                  IAXDIV = 99
                  ILAB2 = ILAB1+MAXLAB-1
               ENDIF
            ELSE
               IAXDIV = MIN(HLABNB(ID,'X'),99)
               ILAB1 = 1
               ILAB2 = IAXDIV
            ENDIF
            NHILAB = IAXDIV
            DO 80 I=ILAB1,ILAB2
               CALL HLGNXT(ID,I,HILABS(I-ILAB1+1),'X')
   80       CONTINUE
            CALL IZLBL
            CHAX0 = CHAXI
            CHAXI = 'TNMC'//CHAX0
         ENDIF
      ENDIF
*
   90 NCHX=LENOCC(CHAXI)
      ILT=MAX(IGTYP-1000*IGXY,1)
      CALL ISLN(ILT)
      CALL IGAXIS(XX(1),XX(3),YY(1),YY(3),BMIN,BMAX,IAXDIV,
     +            CHAXI(1:NCHX))
*
      CALL ISCLIP(1)
      CALL ISLN(ILTYP)
      CALL IGSET('TXCI',RTXCI)
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('LWID',RLWID)
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
*
      END
