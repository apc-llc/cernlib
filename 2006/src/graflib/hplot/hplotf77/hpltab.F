*
* $Id: hpltab.F,v 1.5 1998/12/07 13:47:05 couet Exp $
*
* $Log: hpltab.F,v $
* Revision 1.5  1998/12/07 13:47:05  couet
* - set 2siz is available again. It define the size of the charecters when
* 2D histos are plotted with options TEXT of CHAR. The size is defined in
* percent of the bin heigh.
*
* Revision 1.4  1998/12/02 08:57:39  couet
* - clean up: commented lines of code removed
*
* Revision 1.3  1998/01/30 15:27:20  couet
* - previous commit was wrong.
*
* Revision 1.2  1998/01/30 15:25:18  couet
* - APOLLO version removed
*
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :          10/11/95  09.37.14  by  O.Couet
*-- Author :    O.Couet   04/09/89
      SUBROUTINE HPLTAB(IDD,IPAR,PAR,CHOPTI)
*.===========>
*.
*.   This routine draws a table with the 2D histo IDD
*. according to the value of CHOPT
*.
*. _Input parameters:
*.
*.  INTEGER IDD : Histogram ID.
*.  REAL PAR : Array of real parameters
*.  CHARACTER CHOPT : Options
*.
*. +-------+---------------------------------------------------------+---------+
*. | CHOPT |                     Corresponding values of PAR         | Default |
*. +-------+---------------------------------------------------------+---------+
*. | SCAT  | Points (scatter plot)                                   |         |
*. |       |   PAR(1)  = Marker type                                 |   1.    |
*. |       |   PAR(2)  = Maximum number of random points per cell    |   50.   |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. | BOX   | Boxes                                                   |         |
*. |       |   PAR(1)  = ...                                         |   ...   |
*. |       |   PAR(2)  = ...                                         |   ...   |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. | ARR   | Arrows                                                  |         |
*. |       |   PAR(1)  = ...                                         |   ...   |
*. |       |   PAR(2)  = ...                                         |   ...   |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. | CONT  | Contour plot                                            |         |
*. |       |   PAR(1)  = Nlevel (min=2 max=50)                       |   20.   |
*. |       |   PAR(2)  = 0 use colour to distinguish contours        |   0.    |
*. |       |             1 use line style to distinguish contours    |         |
*. |       |             2 line style and colour are the same for all|         |
*. |       |               contours                                  |         |
*. |       |             3 draw the contour with fill colored fill   |         |
*. |       |               area                                      |         |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. |       |   PAR(7)  =   --+                                       |         |
*. |       |     .           |                                       |         |
*. |       |     .           +-> Level to be drawn                   |         |
*. |       |     .           |                                       |         |
*. |       |   PAR(NPAR) = --+                                       |         |
*. +-------+---------------------------------------------------------+---------+
*. | COL   | COLour plot                                             |         |
*. |       |   PAR(1)  = 0 use the standard 8 colours                |   0.    |
*. |       |             1 use versacolor pattern table              |         |
*. |       |   PAR(2)  = ...                                         |         |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. | TEXT  | Table (Text)                                            |         |
*. |       |   PAR(1)  = Text size                                   |   0.3   |
*. |       |   PAR(2)  = ...                                         |         |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. | CHAR  | Character, the contains is one single character         |         |
*. |       |   PAR(1)  = Text size                                   |   0.3   |
*. |       |   PAR(2)  = ...                                         |         |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. | LEGO  | Lego. Options 1 to 2 see IGTABL                         |         |
*. |       |   PAR(1)  = THETA                                       |   30.   |
*. |       |   PAR(2)  = PHI                                         |   30.   |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. | SURF  | Surface. Options 1 to 4 see IGTABL                      |         |
*. |       |   PAR(1)  = THETA                                       |   30.   |
*. |       |   PAR(2)  = PHI                                         |   30.   |
*. |       |   PAR(3)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(4)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(5)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(6)  = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. | SAME  | Option SAME (or S)                                      |         |
*. +-------+---------------------------------------------------------+---------+
*. |   +   | For stacked histogram (legos)                           |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'FB'  | With LEGO or SURFACE, it suppress the Front-Box         |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'BB'  | With LEGO or SURFACE, it suppress the Back-Box          |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'AX'  | Suppress the axis drawing on 3D representations         |         |
*. +-------+---------------------------------------------------------+---------+
*.
*..==========> (O.Couet)
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcfitd.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl3.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl11.inc"
#include "hplot/hpl13.inc"
#include "hplot/hpl14.inc"
#include "hplot/quest.inc"
      INTEGER ICMAP(MXLIST)
      COMMON /HPLYZO/ Y1ZOOM,Y2ZOOM
      CHARACTER*(*) CHOPTI
      DIMENSION PAR(*)
      CHARACTER*4 NAME
      CHARACTER*32 CHOPT,CHOPT2
      DIMENSION COORD(6),PAR2(NMAX)
      EQUIVALENCE (XLINE(1),PAR2(1))
      LOGICAL D3,D2,LOLOG,LOCOL,LOCOLZ
      LOGICAL LVALUE, LPARAM, LUTSAV
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=1)
      REAL SS
#endif
      REAL VVV (2), XXX, YYY
      EQUIVALENCE (XXX, VVV(1)), (YYY, VVV(2))
      PARAMETER (NVFIT = 50)
      REAL VFIT (NVFIT, NVFIT)
      EXTERNAL HQF
      SAVE NIDS
      DATA NIDS /1/
*._____________________________
*
      NPAR=IPAR
      IF(NPAR.GT.NMAX)NPAR=NMAX
      IF(NPAR.GE.2)THEN
         PAR2(1)=PAR(1)
         PAR2(2)=PAR(2)
      ELSE
         PAR2(1)=0.
         PAR2(2)=0.
      ENDIF
*
      CALL IGQ('LWID',RLWID)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('FASI',RFASI)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('PMCI',RPMCI)
      CALL IGQ('BORD',RBORD)
      CALL IGQ('MTYP',RMK)
      CALL IGQ('TXCI',RTXCI)
*
*              Determine the options
*
      D3     = .FALSE.
      D2     = .FALSE.
      LOLOG  = .FALSE.
      LOCOL  = .FALSE.
      LOCOLZ = .FALSE.
      CHOPT  = CHOPTI
      IF (IDEF2D.GT.0.AND.CHOPT.EQ.' ') THEN
         IF (IDEF2D.EQ.7)  CHOPT = 'BOX'
         IF (IDEF2D.EQ.8)  CHOPT = 'COL'
         IF (IDEF2D.EQ.17) CHOPT = 'CONT'
         IF (IDEF2D.EQ.18) CHOPT = 'TEXT'
         IF (IDEF2D.EQ.19) CHOPT = 'CHAR'
         IF (IDEF2D.EQ.20) CHOPT = 'ARR'
         IF (IDEF2D.EQ.21) CHOPT = 'SCAT'
      ENDIF
      IF ((IDEF2D.GT.0.OR.IDEF1D.GT.0).AND.CHOPT.EQ.' ') THEN
         IDEF                  = IDEF2D
         IF (IDEF.EQ.0)  IDEF  = IDEF1D
         IF (IDEF.EQ.9)  CHOPT = 'SURF'
         IF (IDEF.EQ.10) CHOPT = 'SURF1'
         IF (IDEF.EQ.11) CHOPT = 'SURF2'
         IF (IDEF.EQ.12) CHOPT = 'SURF3'
         IF (IDEF.EQ.13) CHOPT = 'SURF4'
         IF (IDEF.EQ.14) CHOPT = 'LEGO'
         IF (IDEF.EQ.15) CHOPT = 'LEGO1'
         IF (IDEF.EQ.16) CHOPT = 'LEGO2'
      ENDIF
      CHOPT2=' '
      IF(INDEX(CHOPT,'+').NE.0)CHOPT2(22:22)='+'
*
      IOP=INDEX(CHOPT,'POL')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+3)='    '
         CHOPT2(30:32)='POL'
      ENDIF
      IOP=INDEX(CHOPT,'CYL')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+3)='    '
         CHOPT2(30:32)='CYL'
      ENDIF
      IOP=INDEX(CHOPT,'SPH')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+3)='    '
         CHOPT2(30:32)='SPH'
      ENDIF
      IOP=INDEX(CHOPT,'PSD')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+3)='    '
         CHOPT2(30:32)='PSD'
      ENDIF
      IOP=INDEX(CHOPT,'FB')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+1)='  '
         CHOPT2(28:29)='FB '
      ENDIF
      IOP=INDEX(CHOPT,'BB')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+1)='  '
         CHOPT2(26:27)='BB'
      ENDIF
*
      IOP=INDEX(CHOPT,'LEGO')
      IF(IOP.GT.0)THEN
         IF(LOGX.NE.0)CHOPT2(18:19)='GX'
         IF(LOGY.NE.0)CHOPT2(20:21)='GY'
         CHOPT(IOP:IOP+3)='    '
         IF(LOBAR)THEN
            CHOPT2(1:2)='LB'
         ELSE
            CHOPT2(2:2)='L'
         ENDIF
         IF(CHOPT(IOP+4:IOP+4).EQ.'1')CHOPT2(3:3)='1'
         IF(CHOPT(IOP+4:IOP+4).EQ.'2')THEN
            CHOPT2(3:3)='2'
            LOCOL=.TRUE.
         ENDIF
         D3=.TRUE.
         CALL ISPLCI(IHCOL)
         LOHBOX=.FALSE.
         IF (INDEX(CHOPT,'Z').NE.0) LOCOLZ = .TRUE.
      ENDIF
*
      IOP=INDEX(CHOPT,'SURF')
      IF(IOP.GT.0)THEN
         IF(LOGX.NE.0)CHOPT2(18:19)='GX'
         IF(LOGY.NE.0)CHOPT2(20:21)='GY'
         CHOPT(IOP:IOP+3)='    '
         CHOPT2(3:3)='S'
         IF(CHOPT(IOP+4:IOP+4).EQ.'1')THEN
            CHOPT2(4:4)='1'
            LOCOL=.TRUE.
         ENDIF
         IF(CHOPT(IOP+4:IOP+4).EQ.'2')THEN
            CHOPT2(4:4)='2'
            LOCOL=.TRUE.
         ENDIF
         IF(CHOPT(IOP+4:IOP+4).EQ.'3')THEN
            CHOPT2(4:4)='3'
            LOCOL=.TRUE.
         ENDIF
         IF(CHOPT(IOP+4:IOP+4).EQ.'4')THEN
            CHOPT2(4:4)='4'
            LOCOL=.TRUE.
         ENDIF
         D3=.TRUE.
         CALL ISPLCI(IHCOL)
         CALL ISFACI(IHCOL)
         CALL ISLWSC(FLOAT(IHWID))
         LOHBOX=.FALSE.
         IF (INDEX(CHOPT,'Z').NE.0) LOCOLZ = .TRUE.
      ENDIF
*
      IOP=INDEX(CHOPT,'BOX')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+2) = '   '
         CHOPT2(4:4)      = 'B'
         D2               = .TRUE.
         LOLOG            = .TRUE.
         CALL HPLATT(1)
      ENDIF
*
      IOP=INDEX(CHOPT,'COL')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+2)='   '
         CHOPT2(5:7)='COL'
         D2=.TRUE.
         LOLOG=.TRUE.
         IF(IHTYP.EQ.3001)THEN
            PAR2(1)=1.
         ELSE
            PAR2(1)=0.
         ENDIF
         IF (INDEX(CHOPT,'Z').NE.0) LOCOLZ = .TRUE.
      ENDIF
*
      IOP=INDEX(CHOPT,'CONT')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+3)='    '
         IF(PAR2(2).EQ.3.)THEN
            CHOPT2(3:4)='C3'
            LOCOL=.TRUE.
            D3=.TRUE.
            NBLEVL=MIN(MAX(2,INT(PAR2(1))),50)
         ELSE
            CHOPT2(8:8)='C'
            PAR2(2)=PAR2(2)+(FLOAT(IHCOL)/1000.)
            D2=.TRUE.
         ENDIF
      ENDIF
*
      IOP=INDEX(CHOPT,'ARR')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+2)='    '
         CHOPT2(14:14)='R'
         D2=.TRUE.
         CALL ISPLCI(IHCOL)
      ENDIF
*
      IOP=INDEX(CHOPT,'SCAT')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+3)='    '
         CHOPT2(9:9)='P'
         D2=.TRUE.
         LOLOG=.TRUE.
         CALL ISPMCI(IHCOL)
         IF(IHOPTP.EQ.0)THEN
            IF(IHOPTT.EQ.0)THEN
               IF(RMK.NE.1.)CALL ISMK(1)
            ELSE
               IF(RMK.NE.3.)CALL ISMK(3)
            ENDIF
         ENDIF
      ENDIF
*
      IOP=INDEX(CHOPT,'TEXT')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+3)='    '
         CHOPT2(10:10)='T'
         D2=.TRUE.
         CALL ISTXCI(IHCOL)
         PAR2(1) = TVSIZ(9)
      ENDIF
*
      IOP=INDEX(CHOPT,'CHAR')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP+3)='    '
         CHOPT2(11:11)='K'
         D2=.TRUE.
         CALL ISTXCI(IHCOL)
         PAR2(1) = TVSIZ(9)
      ENDIF
*
      IOP=INDEX(CHOPT,'E')
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP)=' '
         CHOPT2(25:25)='E'
         D3=.TRUE.
      ENDIF
*
      IF(CHOPT2.EQ.' ')THEN
         IF(LOTAB.AND.I3.NE.0)THEN
            D2=.TRUE.
            CHOPT2='T'
            CALL ISTXCI(IHCOL)
         ELSE IF(LOCHA)THEN
            D2=.TRUE.
            CHOPT2='K'
            CALL ISTXCI(IHCOL)
         ELSE
            CALL ISPMCI(IHCOL)
         ENDIF
         IF(LOSOFT)THEN
            PAR2(1)=0.
            PAR2(2)=2.
         ELSE
            PAR2(1)=1.
            PAR2(2)=0.
         ENDIF
      ENDIF
*
      IOP=INDEX(CHOPT,'A')
      IOPTAX=0
      IF(IOP.GT.0)THEN
         CHOPT(IOP:IOP)=' '
         IOPTAX=1
      ENDIF
*
      IOP=INDEX(CHOPT,'S')
      IF(IOP.GT.0)THEN
         LOSAME=.TRUE.
      ELSE
         LOSAME=.FALSE.
      ENDIF
      IF(.NOT.D2.AND..NOT.D3)THEN
         D2=.TRUE.
         LOLOG=.TRUE.
         CHOPT2(9:9)='P'
      ENDIF
*
      CHOPT2(13:13)='H'
*
*              Initialise COMMON/HPL13/
*
      CALL VZERO(IOPT,NOPT)
*
      IRET=3
*
*              Loop over all histograms in booking order
*
   10 CALL HLOOP(IDD,'HPLTAB',IRET)
      IF(IRET.EQ.0)GOTO 230
      CALL HDCOFL
*
*              Check for 2 dimensional
*
      IF(I123.EQ.0)GOTO 230
      LCONT=LQ(LCID-1)
      LSCAT=LCONT
*
*              Check if the LOG scales are requested
*
      IF (LOGZ.NE.0) LOGZFL           = .TRUE.
      IF (I26.NE.0)  LOGZFL           = .TRUE.
      IF (LINZ.NE.0) LOGZFL           = .FALSE.
      LOGXFL                          = .FALSE.
      LOGYFL                          = .FALSE.
      IF (LOLOG.AND.LOGX.NE.0) LOGXFL = .TRUE.
      IF (LOLOG.AND.LOGY.NE.0) LOGYFL = .TRUE.
*
*              Fills /HPL9/ with HBOOK information
*              determines XMINI and XMAXI, YMINI and YMAXI.
*
      ICMAX  = IQ(LCID+KNCX)
      XXSIZE = (Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
      XX0    = Q(LCID+KXMIN)
      XMINI  = XX0
      XMAXI  = XX0+(XXSIZE*FLOAT(ICMAX))
*
      IF(I1.NE.0)THEN
         ICMAY  = 0
         YYSIZE = 1.
         YY0    = 0.
         YMINI  = 0.
         YMAXI  = 1.
      ELSE
         ICMAY  = IQ(LCID+KNCY)
         YYSIZE = (Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
         YY0    = Q(LCID+KYMIN)
         YMINI  = YY0
         YMAXI  = YY0+(YYSIZE*FLOAT(ICMAY))
      ENDIF
*
*              Prepare parameter for IGTABL
*
      IF(NPAR.GT.2)THEN
         DO 20 I=3,NPAR
            PAR2(I+4) = PAR(I)
   20    CONTINUE
      ENDIF
      NPAR2 = MAX(NPAR,2)+4
      IF (NPAR.GE.6) THEN
         IF (PAR(5).GT.0.) THEN
            IXMIN = INT(PAR(5)/1000)
            IXMAX = INT(PAR(5)-(1000*IXMIN))
         ELSEIF (PAR(5).LT.0.) THEN
            IXMIN = -PAR(5)
            IXMAX = IQUEST(60)
         ELSE
            IXMIN = 1
            IXMAX = ICMAX
         ENDIF
         IF (PAR(6).GT.0.) THEN
            IYMIN = INT(PAR(6)/1000)
            IYMAX = INT(PAR(6)-(1000*IYMIN))
         ELSEIF (PAR(6).LT.0.) THEN
            IYMIN = -PAR(6)
            IYMAX = IQUEST(61)
         ELSE
            IYMIN = 1
            IYMAX = ICMAY
         ENDIF
         XMINI = XX0+(XXSIZE*FLOAT(IXMIN-1))
         XMAXI = XX0+(XXSIZE*FLOAT(IXMAX))
         YMINI = YY0+(YYSIZE*FLOAT(IYMIN-1))
         YMAXI = YY0+(YYSIZE*FLOAT(IYMAX))
      ENDIF
      IF (XMINI.GE.XMAXI) XMAXI = XMINI+XXSIZE
      IF (YMINI.GE.YMAXI) YMAXI = YMINI+YYSIZE
      IF (LOLOG.AND.LOGX.NE.0) THEN
         IF (XMAXI.LE.0.) GOTO 30
         IF (XMINI.LE.0.) XMINI = 0.1*XXSIZE
         XMINI = LOG10(XMINI)
         XMAXI = LOG10(XMAXI)
      ENDIF
      IF (LOLOG.AND.LOGY.NE.0) THEN
         IF (YMAXI.LE.0.) GOTO 30
         IF (YMINI.LE.0.) YMINI = 0.1*YYSIZE
         YMINI = LOG10(YMINI)
         YMAXI = LOG10(YMAXI)
      ENDIF
      GOTO 40
   30 CALL HBUG('Routine called with zero or negative argument with log
     +scale','HPLOT ',IDD)
      GOTO 230
   40 CONTINUE
      PAR2(3) = XMINI
      PAR2(4) = XMAXI
      PAR2(5) = YMINI
      PAR2(6) = YMAXI
      IF(LOMAX.AND.I1.NE.0)PAR2(5) = SCMAX
*
      FACTOR=1.0
      IF(I19.NE.0)FACTOR=Q(LCID+KSCAL2)
*
* HPLSWN defines the world coordinates and draw the histogram titles.
* The histogram titles drawing need to be postponed (with LOUTIT) after
* the histogram drawing because in case of 3D plots, the axis positions
* is computed in IGTABL.
*
      IF(NIDS.EQ.1)THEN
         CALL IZSAVA
         LUTSAV = LOUTIT
         LOUTIT = .TRUE.
         IF (D3.AND.I1.EQ.0.AND.INDEX(CHOPT2,'C3').EQ.0) THEN
            LO3DPL = .TRUE.
         ELSE
            LO3DPL = .FALSE.
         ENDIF
         CALL HPLSWN
         LOUTIT = LUTSAV
         CALL IZSETA
      ENDIF
*
* Store the primitive identifier in the current HIGZ picture.
*
      IF(I1.NE.0)THEN
         CALL IGPID(1,'1d',IDD,' ')
      ELSE
         CALL IGPID(1,'2d',IDD,' ')
      ENDIF
*
* In case of Contour plots (CHOPTI='CONT'), PAR2(25) could be a level
* to be drawn and should not be changed.
*
      IF(INDEX(CHOPTI,'CONT').EQ.0)THEN
         IF(INDEX(CHOPT,'+').GT.0)THEN
            I=NIDS
            IF(I.GT.NBEL(IHLIST))THEN
               I=MOD(I,NBEL(IHLIST))
               IF(I.EQ.0)I=NBEL(IHLIST)
            ENDIF
            PAR2(25)=FLOAT(ILIST(IHLIST,I))
            NIDS=NIDS+1
         ENDIF
         IF(INDEX(CHOPT,'+').LE.0)THEN
            IF(NIDS.GT.1)THEN
               I=NIDS
               IF(I.GT.NBEL(IHLIST))THEN
                  I=MOD(I,NBEL(IHLIST))
                  IF(I.EQ.0)I=NBEL(IHLIST)
               ENDIF
               PAR2(25)=FLOAT(ILIST(IHLIST,I))
            ELSE
               PAR2(25)=FLOAT(IHCOL)
            ENDIF
            NIDS=1
         ENDIF
         IF(PAR2(25).EQ.1..AND.INDEX(CHOPT2,'L1').NE.0)PAR2(25)=0.
      ENDIF
*
      CALL HPLDES(ID)
*
* Set up the IGTABL log options (if required).
*
      IF(LOGZFL)CHOPT2(16:17)='GZ'
      IF(LOGXFL)CHOPT2(18:19)='GX'
      IF(LOGYFL)CHOPT2(20:21)='GY'
*
*              ZMAX and ZMIN management
*
      IF(I20.NE.0.OR.I21.NE.0.OR.NPAR.GE.4)THEN
         NPAR2=MAX(8,NPAR2)
         IF(I20.NE.0.OR.I21.NE.0)THEN
            IF(I1.NE.0)THEN
               PAR2(7)=HCX(1,1)
               LOOPY=1
            ELSE
               PAR2(7)=HCXY(1,1,1)
               LOOPY=ICMAY
            ENDIF
            PAR2(8)=PAR2(7)
            DO 60 J=1,LOOPY
               DO 50 I=1,ICMAX
                  IF(I1.NE.0)THEN
                     Z=HCX(I,1)
                  ELSE
                     Z=HCXY(I,J,1)
                  ENDIF
                  PAR2(7)=MIN(PAR2(7),Z)
                  PAR2(8)=MAX(PAR2(8),Z)
   50          CONTINUE
   60       CONTINUE
            IF(I21.NE.0)THEN
               IF(I1.NE.0)THEN
                  PAR2(7)=Q(LCID+KMIN1)
                  IF(I20.EQ.0)PAR2(8)=1.1*PAR2(8)
               ELSE
                  PAR2(7)=Q(LCID+KMIN2)
               ENDIF
            ENDIF
            IF(I20.NE.0)THEN
               IF(I1.NE.0)THEN
                  PAR2(8)=Q(LCID+KMAX1)
               ELSE
                  PAR2(8)=Q(LCID+KMAX2)
               ENDIF
            ENDIF
            GOTO 70
         ENDIF
         IF(NPAR.GE.4)THEN
            PAR2(7)=PAR(3)
            PAR2(8)=PAR(4)
         ENDIF
      ENDIF
   70 CONTINUE
*.______________________________________
*
*              If fit parameters exist and are requested, get them.
*
      LVALUE = .FALSE.
      LPARAM = .FALSE.
      IF(INDEX(CHOPTI,'FUNC').NE.0.OR.D2)THEN
         IF(IQ(LCONT-2).EQ.0)GO TO 150
         LFUNC=LQ(LCONT-1)
         IF(LFUNC.EQ.0)GO TO 150
         IF(IQ(LFUNC-2).EQ.0)GO TO 150
*
*        There is an LFUNC bank with function values.
*
         LVALUE = .TRUE.
         LHFIT =LQ(LFUNC-1)
         IF(LHFIT.NE.0)THEN
*
*           There is also an LHFIT bank with function parameters.
*
            IFITTY=IQ(LHFIT+1)
            NFPAR=IQ(LHFIT+2)
            IF(NFPAR.LE.0)THEN
               GO TO 150
            ELSE
               LPARAM = .TRUE.
               NPFITS=IQ(LHFIT+3)
               NOTHER=IQ(LHFIT+4)
               FITCHI=Q(LHFIT+6)
               IF(IFITTY.EQ.4)THEN
                  CALL HQGETF(LHFIT)
               ELSE
                  NP=MIN(NFPAR,35)
                  II=11
                  DO 80 I=1,NP
                     CALL UCOPY(Q(LHFIT+II),SS,NWW)
                     FITPAR(I)=SS
*           Note: FITPAR is only single precision.
                     II=II+NWW
   80             CONTINUE
                  NWERR=IQ(LHFIT-1)-NWW*(NFPAR+NOTHER)
                  IF(NWERR.GT.0)THEN
                     II=IQ(LHFIT-1)-NWERR+1
                     DO 90  I=1,NP
                        CALL UCOPY(Q(LHFIT+II),SS,NWW)
                        FITPAD(I)=SS
                        FITSIG(I)=SS
*              Note: FITSIG is only single precision.
                        II=II+NWW
   90                CONTINUE
                  ELSE
                     CALL VZERO(FITSIG,NP)
                  ENDIF
* Get names if available, otherwise generate from IFITTY.
                  DO 100 I=1,NP
                     FITNAM(I)=' '
  100             CONTINUE
                  IF(IFITTY.EQ.1)THEN
* Polynomial.
                     N1=MAX(NP,10)
                     DO 110 I=1,N1
                        WRITE(FITNAM(I),'(''A'',I1,6X)')I-1
  110                CONTINUE
                     IF(NP.GT.10)THEN
                        DO 120 I=11,NP
                           WRITE(FITNAM(I),'(''A'',I2,5X)')I-1
  120                   CONTINUE
                     END IF
                  ELSE IF(IFITTY.EQ.2)THEN
* Exponential.
                     FITNAM(1)='Constant'
                     FITNAM(2)='Slope'
                  ELSE IF(IFITTY.EQ.3)THEN
* Gaussian.
                     FITNAM(1)='Constant'
                     FITNAM(2)='Mean'
                     FITNAM(3)='Sigma'
                  ELSE IF(IFITTY.NE.4)THEN
                     L=LQ(LHFIT)
  130                CONTINUE
                     IF(L.NE.0)THEN
                        CALL UHTOC(IQ(L-4),4,NAME,4)
                        IF(NAME.EQ.'HFNA')THEN
                           DO 140 I=1,NP
                              CALL UHTOC(Q(L+2*I-1),4,FITNAM(I),8)
  140                      CONTINUE
                        ELSE
                           GO TO 130
                        END IF
                     END IF
                  END IF
               END IF
*        Get covariances if required and when available.
            END IF
         END IF
  150    CONTINUE
      END IF
*.______________________________________
*
*              Draw 3D representations
*
      IF(D3)THEN
         DO 160 I=NPAR2+1,10
            PAR2(I)=0.
  160    CONTINUE
         IF(PAR2(9).EQ.0..AND.PAR2(10).EQ.0..AND.
     +      ICMAX.LT.1000.AND.ICMAY.LT.1000)THEN
            PAR2(9) =FLOAT(1000+ICMAX)
            IF(I1.NE.0)THEN
               PAR2(10)=0.
            ELSE
               PAR2(10)=FLOAT(1000+ICMAY)
            ENDIF
         ENDIF
         CALL HPLAX2(PAR2(11))
         NPAR2=25
*
* Initialize the color level if needed (LOCOL = .TRUE.)
*
         IF(LOCOL)THEN
            IF(CHOPT2(3:4).NE.'C3')NBLEVL=NBEL(IHLIST)
            DO 170 I=1,NBLEVL
               J=MOD(I,NBEL(IHLIST))
               IF(J.EQ.0)J=NBEL(IHLIST)
               PAR2(24+I)=FLOAT(ILIST(IHLIST,J))
  170       CONTINUE
            NPAR2=24+NBLEVL
         ENDIF
*
* Contour with colours. This option needs 3D drawing but 2D axis.
* THETA=90, PHI=0., and NDVX=0.
*
         IF(CHOPT2(3:4).EQ.'C3')THEN
            PAR2(1)     = 90.
            PAR2(2)     = 0.
            PAR2(11)    = 0.
            CHOPT2(3:4) = 'S2'
            D2=.TRUE.
         ENDIF
*
* Suppress the Axis if required
*
         IF(IOPTAX.NE.0)PAR2(11)=0.
*
* Draw the 3D table accoding to CHOPT2
*
         IF(INDEX(CHOPTI,'FUNC').NE.0.AND.LVALUE)THEN
*           Plot function - for details see 2-D.
            CHOPT = CHOPT2
            III = INDEX(CHOPT,'H')
            IF (III .NE. 0) CHOPT (III:III) = ' '
            IF(LPARAM)THEN
               DXXX = (XMAXI - XMINI) / NVFIT
               DYYY = (YMAXI - YMINI) / NVFIT
               DO 190 IX = 1, NVFIT
                  XXX = XMINI + (IX - 0.5) * DXXX
                  DO 180 IY = 1, NVFIT
                     YYY = YMINI + (IY - 0.5) * DYYY
                     IF (IFITTY.EQ.4) VFIT (IX, IY) = HQF (VVV)
  180             CONTINUE
  190          CONTINUE
               PAR2 (9)  = 1000 + NVFIT
               PAR2 (10) = 1000 + NVFIT
               CALL IGTABL(NVFIT,NVFIT,VFIT,NPAR2,PAR2,CHOPT)
            ELSE
               CALL IGTABL(ICMAX,ICMAY,Q(LFUNC+5),NPAR2,PAR2,CHOPT)
            END IF
         ELSE
            CALL IGTABL(ICMAX,ICMAY,FLOAT(LSCAT),NPAR2,PAR2,CHOPT2)
         ENDIF
      ENDIF
*.______________________________________
*
*              Draw 2D representations
*
      IF(D2)THEN
*
* Draw the 2D table accoding to CHOPT2
*
         IF(INDEX(CHOPTI,'FUNC').EQ.0)THEN
            IF(IQ(LCONT+KNOENT).NE.0.AND..NOT.D3)THEN
               CALL IGTABL(ICMAX,ICMAY,FLOAT(LSCAT),NPAR2,PAR2,CHOPT2)
               CALL HPLU(IDD,2)
            ENDIF
         ENDIF
*
* Draw the 2D fit
*
         IF(INDEX(CHOPTI,'HIST').EQ.0.AND.LVALUE)THEN
            IF(INDEX(CHOPTI,'FUNC').NE.0)THEN
               CHOPT = CHOPT2
            ELSE
               PAR2(1)=20.
               PAR2(2)=0.
               CHOPT = 'C'
            ENDIF
            III = INDEX(CHOPT,'H')
            IF (III .NE. 0) CHOPT (III:III) = ' '
            IF(LPARAM)THEN
*
* Plot function from parameters.
*
*                 IFITTY = 1 --> Polynomial  (not implemented for 2-D)
*                 IFITTY = 2 --> Exponential (not implemented for 2-D)
*                 IFITTY = 3 --> Gaussian    (not implemented for 2-D)
*                 IFITTY = 4 --> Multiquadric
*
               DXXX = (XMAXI - XMINI) / NVFIT
               DYYY = (YMAXI - YMINI) / NVFIT
               DO 210 IX = 1, NVFIT
                  XXX = XMINI + (IX - 0.5) * DXXX
                  DO 200 IY = 1, NVFIT
                     YYY = YMINI + (IY - 0.5) * DYYY
                     IF (IFITTY.EQ.4) VFIT (IX, IY) = HQF (VVV)
  200             CONTINUE
  210          CONTINUE
               IF (NPAR2 .GE. 10) THEN
                  PAR2 (9)  = 1000 + NVFIT
                  PAR2 (10) = 1000 + NVFIT
               END IF
               CALL IGTABL(NVFIT,NVFIT,VFIT,NPAR2,PAR2,CHOPT)
            ELSE
*
* Plot function from values.
*
               CALL IGTABL(ICMAX,ICMAY,Q(LFUNC+5),NPAR2,PAR2,CHOPT)
            END IF
         END IF
*
* Draw the 2D Axis
*
         IF(.NOT.LOSAME) THEN
            COORD(1) = XMINI
            COORD(3) = XMAXI
            COORD(4) = YMINI
            COORD(5) = YMAXI
            COORD(2) = COORD(1)
            COORD(6) = COORD(4)
            IF(IOPTAX.EQ.0)THEN
               LOZOOM = .TRUE.
               IF(NPAR.GE.6)THEN
                  NCMIN  = IXMIN
                  NCMAX  = IXMAX
                  Y1ZOOM = IYMIN
                  Y2ZOOM = IYMAX
               ELSE
                  NCMIN  = 1
                  NCMAX  = ICMAX
                  Y1ZOOM = 1
                  Y2ZOOM = ICMAY
               ENDIF
               CALL HPLAXI(COORD(1),COORD(4),1)
               LOZOOM = .FALSE.
            ENDIF
         ENDIF
      ENDIF
*
*              Draw the color map
*
      IF (LOCOLZ) THEN
         CHOPT2 = ' '
*
* Set the color indeces
*
         CALL IGQ('NCOL',RVAL)
         INBCOL = INT(RVAL)
         IF (INBCOL.GT.8) THEN
            IC1 = 8
            IC2 = INBCOL
         ELSE
            IC1 = 0
            IC2 = 7
         ENDIF
*
* Axis font
*
         KFONT  = IABS(IHFONT(3))
         IAXFON = KFONT/10
         IAXPRE = MOD(KFONT,10)
         IF(IHFONT(3).LT.0)IAXFON = -IAXFON
         CALL ISTXFP(IAXFON,IAXPRE)
*
* The color map is drawn in cm coordinates.
*
         NTOLD = NTWIN
         CALL ISELNT(1)
         IF (ICOLMP.EQ.2) THEN
            X1MAP = XLOW
            X2MAP = XHIGH
            Y1MAP = YHIGH+CMMG
            Y2MAP = YHIGH+CWID+CMMG
            CHOPT2(1:1) = 'H'
            CHOPT2(8:8) = 'T'
         ELSEIF (ICOLMP.EQ.3) THEN
            X1MAP = XLOW-CWID-CMMG
            X2MAP = XLOW-CMMG
            Y1MAP = YLOW
            Y2MAP = YHIGH
            CHOPT2(1:1) = 'R'
         ELSEIF (ICOLMP.EQ.4) THEN
            X1MAP = XLOW
            X2MAP = XHIGH
            Y1MAP = YLOW-CWID-CMMG
            Y2MAP = YLOW-CMMG
            CHOPT2(1:1) = 'H'
         ELSE
            X1MAP = XHIGH+CMMG
            X2MAP = XHIGH+CWID+CMMG
            Y1MAP = YLOW
            Y2MAP = YHIGH
         ENDIF
*
         CALL IGSET('LASI',TVSIZ(7))
         CALL IGSET('LAOF',CLVAL)
         Z1          = RQUEST(11)
         Z2          = RQUEST(12)
         CHOPT2(2:3) = 'CA'
         IF (LOGZFL) THEN
            Z1          = 10.**Z1
            Z2          = 10.**Z2
            CHOPT2(2:4) = 'CAG'
         ENDIF
         IF (D3) THEN
            NBCOL=MIN(NBEL(IHLIST),MXLIST)
            DO 220 I=1,NBCOL
               ICMAP(I) = ILIST(IHLIST,i)
  220       CONTINUE
            CHOPT2(5:5) = 'P'
            CALL IGCOLM (X1MAP,X2MAP,Y1MAP,Y2MAP,J,ICMAP,Z1,Z2,CHOPT2)
         ELSE
            CALL IGCOLM(X1MAP,X2MAP,Y1MAP,Y2MAP,IC1,IC2,Z1,Z2,CHOPT2)
         ENDIF
         CALL ISELNT(NTOLD)
      ENDIF
*
      IRET=2
      CALL IGSET('TXCI',RTXCI)
      IF((LOSTAT).AND.(.NOT.LOSAME))CALL HPLSTA(IDD,'HIST',1)
      IF((LOTIC).AND.(.NOT.LOSAME).AND.D2)
     +                              CALL HPLWIR(' ',BIGP,BIGP,'TICK')
      CALL HPLFIL
      CALL HPLDAT
      IF (.NOT.LOUTIT.AND..NOT.LOSAME) CALL HPLTIT(' ')
      GOTO 10
*
  230 LOSAME=.FALSE.
      LOHBOX=.TRUE.
      IF(IHOPTP.EQ.0)THEN
         IF(IHOPTT.EQ.0)THEN
            IF(RMK.NE.1.)CALL IGSET('MTYP',RMK)
         ELSE
            IF(RMK.NE.3.)CALL IGSET('MTYP',RMK)
         ENDIF
      ENDIF
      CALL IGSET('LWID',RLWID)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('FASI',RFASI)
      CALL IGSET('FACI',RFACI)
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('PMCI',RPMCI)
      CALL IGSET('BORD',RBORD)
*
      END
