*
* $Id: hplset.F,v 1.6 2002/04/04 10:24:34 couet Exp $
*
* $Log: hplset.F,v $
* Revision 1.6  2002/04/04 10:24:34  couet
* - Use IGCOL1 to set the number of axis division for the axis colour palette.
*
* Revision 1.5  2001/03/06 10:32:57  couet
*  The previous fix (Some rounding problem on IEEE machines) introduced problems
*  in case of negative division number. This is now fixed.
*
* Revision 1.4  2000/09/06 09:44:55  couet
* Some rounding problem on IEEE machines. The following small macro is enough
* to demonstrate it:
*
* label 2 5 a b c d e
* set ndvx 10.20
* nul 0 1
*
* On RS6000, Linux and Windows the labels are not visible.
* On these machine 10.20 becomes 10.1999999 and the label list used is 1
* instead of 2. To fix that 0.001 is now added to the number of divisions.
*
* Revision 1.3  1998/12/07 13:47:04  couet
* - set 2siz is available again. It define the size of the charecters when
* 2D histos are plotted with options TEXT of CHAR. The size is defined in
* percent of the bin heigh.
*
* Revision 1.2  1998/12/02 15:05:39  couet
* - 2SIZ not used anymore
*
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :          09/11/95  17.46.23  by  O.Couet
*-- Author :
      SUBROUTINE HPLSET(CASE,ARG)
*.==========>
*.            subroutine to allow the user to define values for
*.            variables within HPLOT
*..=========>
#include "hplot/quest.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl9.inc"
#include "hbook/hcunit.inc"
*
      PARAMETER (NCASE=74, NCASE1=72)
*
      DIMENSION PL4(18),DEFVAL(NCASE1)
      EQUIVALENCE (PL4(1),XSIZ)
      CHARACTER*4 KKASE(NCASE)
      DIMENSION XSHOW(44)
      CHARACTER*32 CHEXP1(28),CHEXP2(44)
      CHARACTER*(*) CASE
*
      DATA DEFVAL/20.,20.,2.0,2.0,1.4,0.4,0.3,
     +            2.0,2.0,0.8,0.2,0.3,0.6,1.5,1.2,0.,0.,0.3,0.2,
     +            0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,
     +            2.0,2.0,0.9,1.,0.03,0.25,0.5,
     +            0.15,1.,3.,1.,2.,1.,1111.,101.,
     +            0.,0.,0.,0.,
     +            1.,1.,1.,1.,1.,1.,
     +            1.,1.,1.,1.,1.,1.,0.5,
     +            2.,2.,2.,2.,2.,
     +            10510.00,10510.00,10510.00,1.,0.5,0.,0./
*
      DATA KKASE/'XSIZ','YSIZ','XMGL','XMGR','XLAB','XVAL','XTIC'
     +,          'YMGL','YMGU','YLAB','YVAL','YTIC','YNPG','YGTI'
     +,          'YHTI','SMGR','SMGU','CMMG','CVAL'
     +,          'KSIZ','GSIZ','TSIZ','ASIZ','CSIZ','PSIZ','VSIZ'
     +,          'SSIZ','2SIZ'
     +,          'XWIN','YWIN','HMAX','PASS','CSHI','BARO','BARW'
     +,          'DASH','DMOD','GRID','CMAP','DATE','FILE','STAT','FIT '
     +,          'HTYP','BTYP','PTYP','FTYP'
     +,          'HCOL','BCOL','PCOL','FCOL','XCOL','YCOL'
     +,          'HWID','BWID','PWID','FWID','XWID','YWID','CWID'
     +,          'TFON','GFON','VFON','LFON','CFON'
     +,          'NDVX','NDVY','NDVZ','FPGN','ERRX'
     +,          '1DEF','2DEF','SHOW','0   '/
*
*      Explanations when SET SHOW is requested :
*      XSIZ, YSIZ, XMGL, XMGR, XLAB, XVAL, XTIC, YMGL,
*      YMGU, YLAB, YVAL, YTIC, YNPG, YGTI, YHTI, SMGR, SMGU, CMMG, CVAL,
*      KSIZ, GSIZ, TSIZ, ASIZ, CSIZ, PSIZ, VSIZ, SSIZ, 2SIZ
*
      DATA CHEXP1/' Size along X    ',' Size along Y    ',' X MarGin Lef
     +t',' X MarGin Right',' distance y axis to LABel',' distance y axis
     + to axis VALues',' X axis TICk marks length',' Y MarGin Low',' Y M
     +arGin Up',' distance x axis to LABel',' distance x axis to axis VA
     +Lues',' Y axis TICk marks length',' Y position for Number of PaGe'
     +,' Y position of Global TItle',' Y position of Histogram TItle','
     +Stat MarGin Right (%)',' Stat MarGin Up (%)',' ColorMap MarGin','
     +distance Color map axis VALues',' Hershey charact. (HPLKEY) SIZe+'
     +,' Global title SIZe',' histogram Title SIZe',' Axis label SIZe','
     + Comment and stat SIZe',' Page number SIZe',' axis Values SIZe','
     +aSterisk SIZe (for functions)',' scatter-plot & table-char. SIZe'/
*
*      Explanations when SET SHOW is requested :
*      XWIN, YWIN, PASS, HMAX, CSHI
*      BARO, BARW, DASH, DMOD, GRID, DATE, FILE, STAT, FIT
*      HTYP, BTYP, PTYP, FTYP
*      HCOL, BCOL, PCOL, FCOL
*      HWID, BWID, PWID, FWID, XWID, YWID, CWID
*      TFON, GFON, VFON, LFON, NDVX, NDVY, NDVZ
*      FPGN, ERRX, 1DEF, 2DEF
*
      DATA CHEXP2/' X space between WINdows',' Y space between WINdows',
     +' Histogram MAXimum for scale',' number of PASS for characters','
     +Character SHIft between 2 pass',' BAR histogram Offset (%)',' BAR
     +histogram Width (%)',' length of basic DASHed segment',' Dash MODe
     + (or type) for lines',' GRID line type',' Color MAP position',' DA
     +TE position',' FILE name position',' STAT values to be plotted','
     +FIT values to be plotted',' Histogram  fill area TYPe',' Box fill
     +area TYPe',' Picture fill area TYPe',' Function fill area TYPe','
     +Histogram fill area COLor',' Box fill area and shading COLor',' Pi
     +cture fill area COLor',' Function fill area COLor',' X axis COLor'
     +,' Y axis COLor',' Histogram line WIDth',' Box line WIDth',' Pictu
     +re line WIDth',' Function line WIDth',' X ticks WIDth',' Y ticks W
     +IDth',' Color map WIDth',' Text (and Title) FONT and PREC',' Globa
     +l title FONT and PREC',' axis Values FONT and PREC',' axis Labels
     +FONT and PREC',' Comment FONT and PREC',' Number of DIVisions for
     +X axis',' Number of DIVisions for Y axis',' Number of DIVisions fo
     +r Z axis',' First PaGe Number',' ERRor on X (% of bin width)',' 1D
     +Plot Option',' 2D Plot Option'/
*._____________________________
*
      IF(CASE.EQ.'*')GO TO 100
*
      IQCASE=0
      IF(CASE(1:1).EQ.'?') THEN
      DO 3 I=1,NCASE
         IF(CASE(2:LENOCC(CASE)).EQ.KKASE(I))THEN
            IQCASE=I
            GOTO 125
         ENDIF
    3 CONTINUE
      CALL IGQ(CASE(2:5),ARG)
      GOTO 999
      ENDIF
*
      DO 5 I=1,NCASE
         IF(CASE.EQ.KKASE(I))THEN
            K=I
            GO TO 6
         ENDIF
    5 CONTINUE
      K=0
*
    6 VAL1=FLOARG(ARG)
*
      IF(CASE.EQ.'*WID')THEN
         IF(VAL1.NE.0.)THEN
            IHWID=INT(VAL1)
         ELSE
            IHWID=1
         ENDIF
         IBWID=IHWID
         IPWID=IHWID
         IFWID=IHWID
         IXWID=IHWID
         IYWID=IHWID
         GOTO 999
      ENDIF
      IF(CASE.EQ.'*FON')THEN
         IF(VAL1.NE.0.)THEN
            IHFONT(1)=INT(VAL1)
         ELSE
            IHFONT(1)=2
         ENDIF
         DO 7 I=2,5
            IHFONT(I)=IHFONT(1)
    7    CONTINUE
         GOTO 999
      ENDIF
      IF(CASE.EQ.'*SIZ')THEN
         IF(VAL1.NE.0.)THEN
            TVSIZ(1)=VAL1
         ELSE
            TVSIZ(1)=0.28
         ENDIF
         DO 8 I=2,9
            TVSIZ(I)=TVSIZ(1)
    8    CONTINUE
         GOTO 999
      ENDIF
      IF(CASE.EQ.'*TYP')THEN
         IHTYP=INT(VAL1)
         IBTYP=IHTYP
         IPTYP=IHTYP
         IFTYP=IHTYP
         GOTO 999
      ENDIF
      IF(CASE.EQ.'*COL')THEN
         IF(VAL1.NE.0.)THEN
            RHCOL=VAL1
         ELSE
            RHCOL=1.
         ENDIF
         IHCOL=INT(RHCOL)
         IF(IHCOL.GT.1000)THEN
            IHBCOL = MOD(IHCOL,1000)/100
            IHCOL  = MOD(IHCOL,100)
            IHTYP  = 2001
         ELSE
            IHBCOL = 0
            IHTYP  = 1000
         ENDIF
         RBCOL     = RHCOL
         IBCOL     = IHCOL
         IBBCOL    = IHBCOL
         IBTYP     = IHTYP
         IPCOL     = IHCOL
         IPBCOL    = IHBCOL
         IPTYP     = IHTYP
         IFCOL     = IHCOL
         IFBCOL    = IHBCOL
         IFTYP     = IHTYP
         IXCOL     = IHCOL
         IYCOL     = IHCOL
         GOTO 999
      ENDIF
*
      IF(K.EQ.NCASE)GOTO 100
      IF(K.EQ.0)THEN
         CALL IGSET(CASE,ARG)
         GOTO 999
      ENDIF
*
*            Margins and tick marks size (1 <=  K <= 19)
*
      IF (K.LE.19) THEN
         XSZOLD = PL4(1)
         YSZOLD = PL4(2)
         IF (VAL1.NE.0.) THEN
            PL4(K) = VAL1
         ELSE
            PL4(K) = DEFVAL(K)
         ENDIF
         IF (K.EQ.1.OR.K.EQ.2) GOTO 80
         GOTO 999
      ENDIF
*
*            Character size (20 <= K <= 28)
*
      IF (K.LE.28) THEN
         IF (VAL1.NE.0.) THEN
            TVSIZ(K-19) = VAL1
         ELSE
            TVSIZ(K-19) = DEFVAL(K)
         ENDIF
         GOTO 999
      ENDIF
*
      IF (K.GT.(NCASE-1)) GOTO 90
*
      ILAB = K-28
      GOTO (31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
     +      51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,
     +      71,72,73,74,75) ILAB
*
*     Real values first are treated
*
*            Window margins
*
   31 CONTINUE
      IF(VAL1.NE.0.)THEN
         XWIND=VAL1
      ELSE
         XWIND=DEFVAL(K)
      ENDIF
      GOTO 999
*
   32 CONTINUE
      IF(VAL1.NE.0.)THEN
         YWIND=VAL1
      ELSE
         YWIND=DEFVAL(K)
      ENDIF
      GOTO 999
*
*            Histogram maximum
*
   33 CONTINUE
      IF(VAL1.GT.1.)THEN
         CALL HBUG(' HMAX must be .LE. 1','HPLSET',0)
         GOTO 999
      ENDIF
      IF(VAL1.NE.0.)THEN
         SCMAX=VAL1
         LOMAX=.TRUE.
      ELSE
         SCMAX=DEFVAL(K)
         LOMAX=.FALSE.
      ENDIF
      GOTO 999
*
*            Boldface characters
*
   34 CONTINUE
      CPASS=VAL1
      CALL IGSET('PASS',CPASS)
      GOTO 999
*
   35 CONTINUE
      IF(VAL1.NE.0.)THEN
         CSHIFT=VAL1
      ELSE
         CSHIFT=DEFVAL(K)
      ENDIF
      CALL IGSET('CSHI',CSHIFT)
      GOTO 999
*
*            Offset for "Bar" histogram
*
   36 CONTINUE
      IF(VAL1.GT.1.)THEN
         CALL HBUG(' BARO must be .LE. 1','HPLSET',0)
         GOTO 999
      ENDIF
      ROFFS=VAL1
      CALL IGSET('BARO',ROFFS)
      GOTO 999
*
*            Width of "Bar"
*
   37 CONTINUE
      IF(VAL1.GT.1.)THEN
         CALL HBUG(' BARW must be .LE. 1','HPLSET',0)
         GOTO 999
      ENDIF
      IF(VAL1.NE.0.)THEN
         RWDTH=VAL1
      ELSE
         RWDTH=DEFVAL(K)
      ENDIF
      CALL IGSET('BARW',RWDTH)
      GOTO 999
*
*            Length of basic dashed segment
*
   38 CONTINUE
      IF(VAL1.NE.0.)THEN
         DLEN=VAL1
      ELSE
         DLEN=DEFVAL(K)
      ENDIF
      DLENN=DLEN*XYTWN
      CALL IGSET('BASL',DLENN)
      GOTO 999
*
*     Integer values then are treated
*
*            Line type
*
   39 CONTINUE
      IF(VAL1.NE.0.)THEN
         ILTYP=INT(VAL1)
         LTYPFL=.FALSE.
      ELSE
         ILTYP=INT(DEFVAL(K))
         LTYPFL=.TRUE.
      ENDIF
      CALL ISLN(ILTYP)
      GOTO 999
*
*            Line type of the grid
*
   40 CONTINUE
      IGTYP=INT(VAL1)
      IF(IGTYP.LT.1)IGTYP=3
      GOTO 999
*
*            Color map position
*
   41 CONTINUE
      ICOLMP=INT(VAL1)
      IF(ICOLMP.LT.1.OR.ICOLMP.GT.4)ICOLMP=1
      GOTO 999
*
*            Date position
*
   42 CONTINUE
      IDATE=INT(VAL1)
      IF(IDATE.LT.1.OR.IDATE.GT.4)IDATE=1
      GOTO 999
*
*            File name position
*
   43 CONTINUE
      IFILE=INT(VAL1)
      IF(IFILE.LT.1.OR.IFILE.GT.4)IFILE=1
      GOTO 999
*
*            STAT Values
*
   44 CONTINUE
      IF (ABS(VAL1).GT.1111111.) VAL1=1111111.
      ISTAT=INT(VAL1)
      IF(ISTAT.LT.1)ISTAT=1111
      GOTO 999
*
*            FIT Values
*
   45 CONTINUE
      IF (ABS(VAL1).GT.111.) VAL1=111.
      IFIT=INT(VAL1)
      IF(IFIT.LT.1)IFIT=101
      GOTO 999
*
*            Histogram hatch type
*
   46 CONTINUE
      IHTYP=INT(VAL1)
      GOTO 999
*
*            Box hatch type
*
   47 CONTINUE
      IBTYP=INT(VAL1)
      GOTO 999
*
*            Picture box hatch type
*
   48 CONTINUE
      IPTYP=INT(VAL1)
      GOTO 999
*
*            Function hatch type
*
   49 CONTINUE
      IFTYP=INT(VAL1)
      GOTO 999
*
*            Histogram color
*      (Interior Style is set to 1 (solid) by default and
*       must be reset (with SET HTYP) if user doesn't agree.)
*
   50 CONTINUE
      IF(VAL1.NE.0.)THEN
         RHCOL = VAL1
      ELSE
         RHCOL = DEFVAL(K)
      ENDIF
      IHCOL  = INT(RHCOL)
      IHLIST = MAX(INT(10.*(RHCOL-IHCOL)),1)
      IF(IHCOL.EQ.0)IHCOL = 1
      IF(IHCOL.GT.1000)THEN
         IHBCOL = MOD(IHCOL,1000)/100
         IHCOL  = MOD(IHCOL,100)
         IHTYP  = 2001
      ELSE
         IHBCOL = 0
         IHTYP  = 1000
      ENDIF
      GOTO 999
*
*            Box color
*
   51 CONTINUE
      IF(VAL1.NE.0.)THEN
         RBCOL = VAL1
      ELSE
         RBCOL = DEFVAL(K)
      ENDIF
      IBCOL  = INT(RBCOL)
      ISBCOL = INT(10.*(RBCOL-IBCOL)+0.01)
      ISSCOL = NINT(100.*(RBCOL-IBCOL)-10.*ISBCOL)
      IF(IBCOL.GT.1000)THEN
         IBBCOL = MOD(IBCOL,1000)/100
         IBCOL  = MOD(IBCOL,100)
         IBTYP  = 2001
      ELSE
         IBBCOL = 0
         IBTYP  = 1000
      ENDIF
      GOTO 999
*
*            Picture box color
*
   52 CONTINUE
      IF(VAL1.NE.0.)THEN
         IPCOL = INT(VAL1)
      ELSE
         IPCOL = INT(DEFVAL(K))
      ENDIF
      IF(IPCOL.GT.1000)THEN
         IPBCOL = MOD(IPCOL,1000)/100
         IPCOL  = MOD(IPCOL,100)
         IPTYP  = 2001
      ELSE
         IPBCOL = 0
         IPTYP  = 1000
      ENDIF
      GOTO 999
*
*            Function color
*
   53 CONTINUE
      IF(VAL1.NE.0.)THEN
         IFCOL = INT(VAL1)
      ELSE
         IFCOL = INT(DEFVAL(K))
      ENDIF
      IF(IFCOL.GT.1000)THEN
         IFBCOL = MOD(IFCOL,1000)/100
         IFCOL  = MOD(IFCOL,100)
         IFTYP  = 2001
      ELSE
         IFBCOL = 0
         IFTYP  = 1000
      ENDIF
      GOTO 999
*
*            X axis color
*
   54 CONTINUE
      IXCOL=INT(VAL1)
      GOTO 999
*
*            Y axis color
*
   55 CONTINUE
      IYCOL=INT(VAL1)
      GOTO 999
*
*            Histogram line width
*
   56 CONTINUE
      IHWID=MAX(INT(VAL1),1)
      GOTO 999
*
*            Box line width
*
   57 CONTINUE
      IBWID=MAX(INT(VAL1),1)
      GOTO 999
*
*            Picture box line width
*
   58 CONTINUE
      IPWID=MAX(INT(VAL1),1)
      GOTO 999
*
*            Function line width
*
   59 CONTINUE
      IFWID=MAX(INT(VAL1),1)
      GOTO 999
*
*            X axis ticks width
*
   60 CONTINUE
      IXWID=MAX(INT(VAL1),1)
      GOTO 999
*
*            Y axis ticks width
*
   61 CONTINUE
      IYWID=MAX(INT(VAL1),1)
      GOTO 999
*
*            Color map width
*
   62 CONTINUE
      IF(VAL1.GT.0.)THEN
         CWID=VAL1
      ELSE
         CWID=DEFVAL(K)
      ENDIF
      GOTO 999
*
*            Title and text font and precision
*
   63 CONTINUE
      IF(VAL1.NE.0.)THEN
         IHFONT(1)=INT(VAL1)
      ELSE
         IHFONT(1)=INT(DEFVAL(K))
      ENDIF
      GOTO 999
*
*            Global title font and precision
*
   64 CONTINUE
      IF(VAL1.NE.0.)THEN
         IHFONT(2)=INT(VAL1)
      ELSE
         IHFONT(2)=INT(DEFVAL(K))
      ENDIF
      GOTO 999
*
*             Axis values font and precision
*
   65 CONTINUE
      IF(VAL1.NE.0.)THEN
         IHFONT(3)=INT(VAL1)
      ELSE
         IHFONT(3)=INT(DEFVAL(K))
      ENDIF
      GOTO 999
*
*             Axis labels font and precision
*
   66 CONTINUE
      IF(VAL1.NE.0.)THEN
         IHFONT(4)=INT(VAL1)
      ELSE
         IHFONT(4)=INT(DEFVAL(K))
      ENDIF
      GOTO 999
*
*             Comment font and precision
*
   67 CONTINUE
      IF(VAL1.NE.0.)THEN
         IHFONT(5)=INT(VAL1)
      ELSE
         IHFONT(5)=INT(DEFVAL(K))
      ENDIF
      GOTO 999
*
*            Number of divisions for X axis
*
   68 CONTINUE
      IF(VAL1.NE.0.)THEN
         IF(VAL1.GE.0)THEN
            RDIVX=VAL1+0.001
         ELSE
            RDIVX=-VAL1+0.001
            RDIVX=-RDIVX
         ENDIF
      ELSE
         RDIVX=DEFVAL(K)
      ENDIF
      GOTO 999
*
*            Number of divisions for Y axis
*
   69 CONTINUE
      IF(VAL1.NE.0.)THEN
         IF(VAL1.GE.0)THEN
            RDIVY=VAL1+0.001
         ELSE
            RDIVY=-VAL1+0.001
            RDIVY=-RDIVY
         ENDIF
      ELSE
         RDIVY=DEFVAL(K)
      ENDIF
      GOTO 999
*
*            Number of divisions for Z axis
*
   70 CONTINUE
      IF(VAL1.NE.0.)THEN
         RDIVZ=VAL1
      ELSE
         RDIVZ=DEFVAL(K)
      ENDIF
      CALL IGCOL1(INT(RDIVZ))
      GOTO 999
*
*            First page number
*
   71 CONTINUE
      IF(VAL1.NE.0.)THEN
         IOPAGE=MAX(1,INT(VAL1))
      ELSE
         IOPAGE=INT(DEFVAL(K))
      ENDIF
      GOTO 999
*
*            % of error on X
*
   72 CONTINUE
      IF(VAL1.NE.0.)THEN
         RERRX=VAL1
      ELSE
         RERRX=DEFVAL(K)
      ENDIF
      GOTO 999
*
*            Default 1D and 2D plot options
*
   73 CONTINUE
      IDEF1D=INT(VAL1)
      GOTO 999
   74 CONTINUE
      IDEF2D=INT(VAL1)
      GOTO 999
*
*            Option SHOW
*
   75 CONTINUE
      WRITE(LERR,1010)
      WRITE(LERR,1000)
      WRITE(LERR,2000)
      WRITE(LERR,3000)
      WRITE(LERR,2000)
      DO 110 I=1,19
         WRITE(LERR,4000) KKASE(I),PL4(I),DEFVAL(I),CHEXP1(I)
  110 CONTINUE
      DO 120 I=20,28
         WRITE(LERR,4000) KKASE(I),TVSIZ(I-19),DEFVAL(I),CHEXP1(I)
  120 CONTINUE
*
  125 XSHOW(1)  = XWIND
      XSHOW(2)  = YWIND
      XSHOW(3)  = SCMAX
      XSHOW(4)  = CPASS
      XSHOW(5)  = CSHIFT
      XSHOW(6)  = ROFFS
      XSHOW(7)  = RWDTH
      XSHOW(8)  = DLEN
      XSHOW(9)  = ILTYP
      XSHOW(10) = IGTYP
      XSHOW(11) = ICOLMP
      XSHOW(12) = IDATE
      XSHOW(13) = IFILE
      XSHOW(14) = ISTAT
      XSHOW(15) = IFIT
      XSHOW(16) = IHTYP
      XSHOW(17) = IBTYP
      XSHOW(18) = IPTYP
      XSHOW(19) = IFTYP
      XSHOW(20) = RHCOL
      XSHOW(21) = RBCOL
      XSHOW(22) = IPCOL
      XSHOW(23) = IFCOL
      XSHOW(24) = IXCOL
      XSHOW(25) = IYCOL
      XSHOW(26) = IHWID
      XSHOW(27) = IBWID
      XSHOW(28) = IPWID
      XSHOW(29) = IFWID
      XSHOW(30) = IXWID
      XSHOW(31) = IYWID
      XSHOW(32) = CWID
      XSHOW(33) = IHFONT(1)
      XSHOW(34) = IHFONT(2)
      XSHOW(35) = IHFONT(3)
      XSHOW(36) = IHFONT(4)
      XSHOW(37) = IHFONT(5)
      XSHOW(38) = RDIVX
      XSHOW(39) = RDIVY
      XSHOW(40) = RDIVZ
      XSHOW(41) = IOPAGE
      XSHOW(42) = RERRX
      XSHOW(43) = IDEF1D
      XSHOW(44) = IDEF2D
*
      IF (IQCASE.NE.0) GOTO 140
*
*
*     Real values
*
      DO 130 I=29,36
         K = I-28
         WRITE(LERR,4000) KKASE(I),XSHOW(K),DEFVAL(I),CHEXP2(K)
  130 CONTINUE
*
*     Integer values
*
      DO 135 I=37,65
         K=I-28
         IF(K.NE.19.AND.
     +      K.NE.20.AND.
     +      K.NE.32)THEN
            WRITE(LERR,5000) KKASE(I)
     +                   ,INT(XSHOW(K)),INT(DEFVAL(I)),CHEXP2(K)
         ELSE
            WRITE(LERR,4000) KKASE(I),XSHOW(K),DEFVAL(I),CHEXP2(K)
         ENDIF
  135 CONTINUE
*
*     Real values
*
      DO 137 I=66,68
         K=I-28
         WRITE(LERR,4000) KKASE(I),XSHOW(K),DEFVAL(I),CHEXP2(K)
  137 CONTINUE
*
*     Integer value
*
      WRITE(LERR,5000) KKASE(69)
     +                ,INT(XSHOW(41)),INT(DEFVAL(69)),CHEXP2(41)
*
      WRITE(LERR,4000) KKASE(70),XSHOW(42),DEFVAL(70),CHEXP2(42)
      WRITE(LERR,5000) KKASE(71)
     +                ,INT(XSHOW(43)),INT(DEFVAL(71)),CHEXP2(43)
      WRITE(LERR,5000) KKASE(72)
     +                ,INT(XSHOW(44)),INT(DEFVAL(72)),CHEXP2(44)
*
      WRITE(LERR,2000)
      CALL IGSET('SHOW',0.)
      GOTO 999
*
  140 CONTINUE
      IF (IQCASE .LE. 19) THEN
          ARG = PL4(I)
      ELSE IF (IQCASE .LE. 27) THEN
          ARG = TVSIZ(I-19)
      ELSE IF (IQCASE .LE. NCASE) THEN
          ARG = XSHOW(I-28)
      ENDIF
      GOTO 999
*
*            Set all default values
*
  100 CONTINUE
      CALL IGSET('*',0.)
      DO 555 I=1,19
         PL4(I)=DEFVAL(I)
  555 CONTINUE
*
      IVSIZE = 4
      LOVERT = .TRUE.
*
      DO 556 I=1,9
         TVSIZ(I) = DEFVAL(I+19)
  556 CONTINUE
      XWIND     = DEFVAL(29)
      YWIND     = DEFVAL(30)
      SCMAX     = DEFVAL(31)
      LOMAX     = .FALSE.
      CPASS     = DEFVAL(32)
      CALL IGSET('PASS',CPASS)
      CSHIFT    = DEFVAL(33)
      CALL IGSET('CSHI',CSHIFT)
      ROFFS     = DEFVAL(34)
      CALL IGSET('BARO',ROFFS)
      RWDTH     = DEFVAL(35)
      CALL IGSET('BARW',RWDTH)
      DLEN      = DEFVAL(36)
      DLENN     = XYTWN*DLEN
      CALL IGSET('BASL',DLENN)
      ILTYP     = INT(DEFVAL(37))
      LTYPFL    = .TRUE.
      CALL ISLN(ILTYP)
      IGTYP     = INT(DEFVAL(38))
      ICOLMP    = INT(DEFVAL(39))
      IDATE     = INT(DEFVAL(40))
      IFILE     = INT(DEFVAL(41))
      ISTAT     = INT(DEFVAL(42))
      IFIT      = INT(DEFVAL(43))
      IHTYP     = INT(DEFVAL(44))
      IBTYP     = INT(DEFVAL(45))
      IPTYP     = INT(DEFVAL(46))
      IFTYP     = INT(DEFVAL(47))
      IHCOL     = INT(DEFVAL(48))
      IHBCOL    = 0
      RBCOL     = DEFVAL(49)
      IBCOL     = INT(RBCOL)
      IBBCOL    = 0
      ISBCOL    = 0
      ISSCOL    = 0
      IPCOL     = INT(DEFVAL(50))
      IPBCOL    = 0
      IFCOL     = INT(DEFVAL(51))
      IFBCOL    = 0
      IXCOL     = INT(DEFVAL(52))
      IYCOL     = INT(DEFVAL(53))
      IHWID     = INT(DEFVAL(54))
      IBWID     = INT(DEFVAL(55))
      IPWID     = INT(DEFVAL(56))
      IFWID     = INT(DEFVAL(57))
      IXWID     = INT(DEFVAL(58))
      IYWID     = INT(DEFVAL(59))
      CWID      = DEFVAL(60)
      IHFONT(1) = INT(DEFVAL(61))
      IHFONT(2) = INT(DEFVAL(62))
      IHFONT(3) = INT(DEFVAL(63))
      IHFONT(4) = INT(DEFVAL(64))
      IHFONT(5) = INT(DEFVAL(65))
      RDIVX     = DEFVAL(66)
      RDIVY     = DEFVAL(67)
      RDIVZ     = DEFVAL(68)
      IOPAGE    = INT(DEFVAL(69))
      RERRX     = DEFVAL(70)
      IDEF1D    = INT(DEFVAL(71))
      IDEF2D    = INT(DEFVAL(72))
*
   80 CONTINUE
*
*         XSIZ and YSIZ have changed, we have to evaluate LOVERT,
*         windowing is reset to (1,1,1), transformation 1 is selected
*
      CALL HPLZON(1,1,1,' ')
      IF (IQUEST(1).NE.0) THEN
         XSIZ = XSZOLD
         YSIZ = YSZOLD
         GOTO 999
      ENDIF
      IF (XSIZ.LE.0..OR.YSIZ.LE.0.) GOTO 90
      IF (XSIZ.LE.YSIZ) THEN
         LOVERT = .TRUE.
      ELSE
         LOVERT = .FALSE.
      ENDIF
      GOTO 999
*
   90 CALL HBUG('Illegal parameter','HPLSET',0)
      GOTO 999
*
 1000 FORMAT(1X,'|',23X,'HPLSET : Current values in use',24X,'|')
 1010 FORMAT(1X,'+',77(1H-),'+')
 2000 FORMAT(1X,'+',12(1H-),'+',15(1H-),'+',15(1H-),'+',32(1H-),'+')
 3000 FORMAT(1X,'|  Parameter | Current value | Default value '
     +,      '|          Explanation           |')
 4000 FORMAT(' |',4X,A4,4X,'|',1X,F9.2,5X,'|',1X,F9.2,5X,'|',A32,'|')
 5000 FORMAT(' |',4X,A4,4X,'|',2X,I8,5X,'|',2X,I8,5X,'|',A32,'|')
*
  999 END
