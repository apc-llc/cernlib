*
* $Id: igaxis.F,v 1.6 1999/10/06 13:51:58 couet Exp $
*
* $Log: igaxis.F,v $
* Revision 1.6  1999/10/06 13:51:58  couet
* - Some optimisations to draw intermediate labels in log scale on vertical axis
* are commented. They produce a wrong labels alignment. The reason why they have
* been put there have not been found i.e. every things looks fine without these
* lines. Nevertheless they are just commented. A test in the PAW suite will be
* added to cover this part of code.
*
* Revision 1.5  1998/12/01 15:48:25  couet
* - Clean up: commented lines of code removed
*
* Revision 1.4  1998/01/27 14:06:11  couet
* *** empty log message ***
*
* Revision 1.3  1996/05/08 14:44:19  couet
* - The test XMIN-XMAX < EPSIL to see if the axis is vertival
*   was wrong because XMIN-MAX can be small (<EPSIL) but the axis
*   horizontal. So the test XMIN=XMAX is put back.
*
* Revision 1.2  1996/04/18 08:40:05  couet
* FLEXE was not initialized
*
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/02 30/05/95  09.20.35  by  O.Couet
*-- Author :
      SUBROUTINE IGAXIS(RXMI,RXMA,RYMI,RYMA,RWMI,RWMA,NNDI,CHOPT)
*.==============>
*.
*.   Draws an axis and/or stores it in the data structure,
*. according to the option selected by IGZSET.
*.
*. _Input parameters:
*.
*.  REAL RXMI      : X origin coordinate in WC space.
*.  REAL RXMA      : X end axis coordinate in WC space.
*.  REAL RYMI      : Y origin coordinate in WC space.
*.  REAL RYMA      : Y end axis coordinate in WC space.
*.  REAL RWMI      : Lowest value for the tick mark
*.                   labels written on the axis.
*.  REAL RWMA      : Highest value for the tick mark labels
*.                   written on the axis.
*.  INTEGER NNDI    : Number of divisions.
*.
*.       NNDI=N1 + 100*N2 + 10000*N3
*.       N1=number of 1st divisions.
*.       N2=number of 2nd divisions.
*.       N3=number of 3rd divisions.
*.           e.g.:
*.           NNDI=0 --> no tick marks.
*.           NNDI=2 --> 2 divisions, one tick mark in the middle
*.                      of the axis.
*.
*.  CHARACTER CHOPT :  Options (see bellow).
*.
*.       CHOPT='G': loGarithmic scale, default is linear.
*.       CHOPT='B': Blank axis. Useful to superpose axis.
*.       CHOPT='A': An arrow is drawn at the end(WMAX) of the axis.
*.
*. Orientation of tick marks on axis.
*. ----------------------------------
*.
*.   Tick marks are normally drawn on the positive side of the axis,
*.   however, if X0=X1, then negative.
*.
*.       CHOPT='+': tick marks are drawn on Positive side. (default)
*.       CHOPT='-': tick mark are drawn on the negative side.
*.       i.e: '+-' --> tick marks are drawn on both sides of the axis.
*.       CHOPT='U': Unlabeled axis, default is labeled.
*.
*. Position of labels on axis.
*. ---------------------------
*.
*.   Labels are normally drawn on side opposite to tick marks.
*.   However:
*.
*.       CHOPT='=': on Equal side
*.
*. Orientation of labels on axis.
*. ------------------------------
*.
*.   Labels are normally drawn parallel to the axis.
*.   However if X0=X1, then Orthogonal
*.           if Y0=Y1, then Parallel
*.
*.       CHOPT='P': Parallel to the axis
*.       CHOPT='O': Orthogonal to the axis (Top to Down).
*.       CHOPT='0': Orthogonal to the axis (Down to Top).
*.
*. Position of labels on tick marks.
*. ---------------------------------
*.
*.   Labels are centered on tick marks.
*.   However , if X0=X1, then they are right adjusted.
*.
*.       CHOPT='R': labels are Right adjusted on tick mark.
*.                    (default is centered)
*.       CHOPT='L': labels are Left adjusted on tick mark.
*.       CHOPT='C': labels are Centered on tick mark.
*.       CHOPT='M': In the Middle of the divisions.
*.       CHOPT='V': Tick mark are drawn verticaly
*.
*. Direction of labels.
*. --------------------
*.
*.   Default is RIGHT
*.
*.       CHOPT='Y': Down
*.
*. Format of labels.
*. -----------------
*.
*.   Blank characters are stripped, and then the
*.   label is correctly aligned. the dot, if last
*.   character of the string, is also stripped, unless
*.
*.       CHOPT='.' Dot obligatory
*.
*.   In the following, we have some parameters, like
*.   tick marks length and characters height (in percentage
*.   of the length of the axis (WC))
*.   The default values are as follows:
*.
*.   Primary tick marks: 3.0 %
*.   Secondary tick marks: 1.5 %
*.   Third order tick marks: .75 %
*.
*.   Length of the arrow: 3.0 %
*.   Width of the arrow: .75 %
*.
*.   Characters height for labels: 2%
*.   Characters spacing (related to height): 40%
*.
*.   Labels offset: 4.0 %
*.
*. Type of labels.
*. ---------------
*.
*.   Labels are normally numeric. However, alphanumeric labels
*.   can be drawn. The value are stored in HILABS (see IGLBL)
*.
*.       CHOPT='T': Alphanumeric labels .
*.
*. Optional grid.
*. --------------
*.
*.       CHOPT='W': cross-Wire
*.
*. Intrinsic parameters.
*. ---------------------
*.
*.       CHOPT='S': Tick marks Size
*.       CHOPT='H': Labels Height
*.       CHOPT='D': Distance labels-axis
*.
*. Axis bining optimization.
*. -------------------------
*.
*.   By default the axis bining is optimized .
*.
*.       CHOPT='N': No bining optimization
*.       CHOPT='I': Integer labelling
*.
*.       CHOPT='X': The axis is not drawn, and the number of divisions,
*.                  the smallest and bigest labels are returned
*.
*.============> (O.Couet C.E.Vandoni N.Cremel-Somon)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#include "higz/hilabs.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      COMMON /AXSIZ/ CHAXSZ
      LOGICAL ZFSAV
      LOGICAL FLEXE,FLEXPO,FLEXNE,INTLOG
      DIMENSION ATICK (3), NN(3),XPL(4),YPL(4)
      DIMENSION IOPT(25)
      EQUIVALENCE (IOPTG,IOPT(1))  , (IOPTB,IOPT(2))
      EQUIVALENCE (IOPTA,IOPT(3))  , (IOPTV,IOPT(4))
      EQUIVALENCE (IOPTPL,IOPT(5)) , (IOPTN,IOPT(6))
      EQUIVALENCE (IOPTU,IOPT(7))  , (IOPTP,IOPT(8))
      EQUIVALENCE (IOPTO,IOPT(9))  , (IOPTR,IOPT(10))
      EQUIVALENCE (IOPTL,IOPT(11)) , (IOPTC,IOPT(12))
      EQUIVALENCE (IOPTEQ,IOPT(13)), (IOPTDO,IOPT(14))
      EQUIVALENCE (IOPTH,IOPT(15)) , (IOPTD,IOPT(16))
      EQUIVALENCE (IOPTY,IOPT(17)) , (IOPTT,IOPT(18))
      EQUIVALENCE (IOPTW,IOPT(19)) , (IOPTS,IOPT(20))
      EQUIVALENCE (IOPTNN,IOPT(21)), (IOPTI,IOPT(22))
      EQUIVALENCE (IOPTM,IOPT(23)) , (IOPT0,IOPT(24))
      EQUIVALENCE (IOPTX,IOPT(25))
      CHARACTER*(*) CHOPT
      CHARACTER*32 LABEL
      CHARACTER*8 CODED
      CHARACTER*36 CHTEMP
*     Set the labels precision
      PARAMETER(NCAR=5)
      DATA EPSIL /0.00001/
*.______________________________________
*
      XMIN=RXMI
      XMAX=RXMA
      YMIN=RYMI
      YMAX=RYMA
      WMIN=RWMI
      WMAX=RWMA
      NDIV=NNDI
*
      CALL UOPTC(CHOPT,'GBAV+-UPORLC=.HDYTWSNIM0X',IOPT)
#if defined(CERNLIB_MAIL)
      CHOPTM=CHOPT
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZAXIS(XMIN,XMAX,YMIN,YMAX,WMIN,WMAX,NDIV,IOPT)
#endif
      IF((.NOT.GFLAG).AND.(.NOT.PFLAG))RETURN
      ZFSAV=ZFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
*              Set the grid lenght
*
      IGRITY=ILN
      GRIL1=0.
      IF(IOPTW.NE.0)THEN
         GRILEN=RAWL
         GRIL1=GRILEN
      ENDIF
*
*              Determine number of divisions 1, 2 and 3
*
      N     = NDIV
      N3A   = N/10000
      N     = N-N3A*10000
      N2A   = N/100
      N1A   = N-N2A*100
      NN(3) = MAX(N3A,1)
      NN(2) = MAX(N2A,1)*NN(3)
      NN(1) = MAX(N1A,1)*NN(2)+1
      LOOP  = NN(1)
*
*              Axis bining optimization is ignored if:
*                - the first and the last label are equal
*                - the number of divisions is 0
*                - less than 1 primary division is requested
*                - logarithmic scale is requested
*
      IF((WMIN.EQ.WMAX).OR.(NDIV.EQ.0).OR.
     +   (N1A.LE.1).OR.(IOPTG.NE.0))THEN
         IOPTNN = 1
         IOPTI  = 0
      ENDIF
*
*              Axis bining optimization
*
      IF(((WMAX-WMIN).LT.1.).AND.(IOPTI.NE.0))THEN
         CALL IGERR('Option "I" not available','IGAXIS')
         IOPTI=0
      ENDIF
      IF(IOPTNN.EQ.0.OR.IOPTI.NE.0)THEN
*
* Primary divisions optimization
*
*    The optimization provide by IGAXI0 is better than the one provide
* by IGAXI4. When integer labelling is required, IGAXI0 is invoke first
* and only if the result is not an integer labelling, IGAXI4 is invoked.
*
         CALL IGAXI0(WMIN,WMAX,N1A,BL,BH,NB,BWID)
         IF(IOPTI.NE.0)THEN
            IF(BL.NE.FLOAT(INT(BL)).OR.BWID.NE.FLOAT(INT(BWID)))THEN
               CALL IGAXI4(WMIN,WMAX,N1A,BL,BH,NB,BWID)
            ENDIF
         ENDIF
         IF((WMIN-BL).GT.EPSIL)THEN
            BL = BL+BWID
            NB = NB-1
         ENDIF
         IF((BH-WMAX).GT.EPSIL)THEN
            BH = BH-BWID
            NB = NB-1
         ENDIF
         IF(XMAX.EQ.XMIN)THEN
            RTYW  = (YMAX-YMIN)/(WMAX-WMIN)
            XXMIN = XMIN
            XXMAX = XMAX
            YYMIN = RTYW*(BL-WMIN)+YMIN
            YYMAX = RTYW*(BH-WMIN)+YMIN
         ELSE
            RTXW  = (XMAX-XMIN)/(WMAX-WMIN)
            XXMIN = RTXW*(BL-WMIN)+XMIN
            XXMAX = RTXW*(BH-WMIN)+XMIN
            IF(YMAX.EQ.YMIN)THEN
               YYMIN = YMIN
               YYMAX = YMAX
            ELSE
               ALFA  = (YMAX-YMIN)/(XMAX-XMIN)
               BETA  = (YMIN*XMAX-YMAX*XMIN)/(XMAX-XMIN)
               YYMIN = ALFA*XXMIN+BETA
               YYMAX = ALFA*XXMAX+BETA
            ENDIF
         ENDIF
         WMIN = BL
         WMAX = BH
*
* Secondary divisions optimization
*
         NB2 = N2A
         IF(IOPTNN.EQ.0.AND.N2A.GT.1.AND.BWID.GT.0.)THEN
            CALL IGAXI0(WMIN,WMIN+BWID,N2A,BL2,BH2,NB2,BWID2)
         ENDIF
*
* Tertiary divisions optimization
*
         NB3 = N3A
         IF(IOPTNN.EQ.0.AND.N3A.GT.1.AND.BWID2.GT.0.)THEN
            CALL IGAXI0(BL2,BL2+BWID2,N3A,BL3,BH3,NB3,BWID3)
         ENDIF
*
         N1AOLD = N1A
         NN1OLD = NN(1)
         N1A    = NB
         NN(3)  = MAX(NB3,1)
         NN(2)  = MAX(NB2,1)*NN(3)
         NN(1)  = MAX(N1A,1)*NN(2)+1
         LOOP   = NN(1)
      ENDIF
*
*              Coordinates are normalized
*
      RATIO1 = (RVXMAX-RVXMIN)/(RWXMAX-RWXMIN)
      RATIO2 = (RVYMAX-RVYMIN)/(RWYMAX-RWYMIN)
      X0     = RATIO1*(XMIN-RWXMIN)+RVXMIN
      X1     = RATIO1*(XMAX-RWXMIN)+RVXMIN
      Y0     = RATIO2*(YMIN-RWYMIN)+RVYMIN
      Y1     = RATIO2*(YMAX-RWYMIN)+RVYMIN
      IF(XMIN.NE.XMAX)THEN
         GRILEN = RATIO2*(GRIL1-RWYMIN)+RVYMIN-Y0
      ELSE
         GRILEN = RATIO1*(GRIL1-RWXMIN)+RVXMIN-X0
      ENDIF
      IF((IOPTNN.EQ.0).OR.(IOPTI.NE.0))THEN
         XX0 = RATIO1*(XXMIN-RWXMIN)+RVXMIN
         XX1 = RATIO1*(XXMAX-RWXMIN)+RVXMIN
         YY0 = RATIO2*(YYMIN-RWYMIN)+RVYMIN
         YY1 = RATIO2*(YYMAX-RWYMIN)+RVYMIN
         IF(XMIN.NE.XMAX)THEN
            GRILEN = RATIO2*(GRIL1-RWYMIN)+RVYMIN-YY0
         ELSE
            GRILEN = RATIO1*(GRIL1-RWXMIN)+RVXMIN-XX0
         ENDIF
      ENDIF
*
      IF((X0.EQ.X1).AND.(Y0.EQ.Y1))THEN
         CALL IGERR('Length of axis is zero','IGAXIS')
         GOTO 220
      ENDIF
*
*              Return WMIN, WMAX and the number of primary divisions
*
      IF(IOPTX.NE.0)THEN
         RWMI = WMIN
         RWMA = WMAX
         NNDI = N1A
         GOTO 220
      ENDIF
*
      ICTR   = INTR
      CHSIZ  = RCHH
      CHUPXV = RCHUX
      CHUPYV = RCHUY
      IALH   = ITXALH
      IALV   = ITXALV
      ILNV   = ILN
      ICLIPV = ICLIP
*
      CALL ISCLIP(0)
      CALL ISELNT(0)
      CALL ISLN(1)
*
      TICK   = .03
      ARROL  = 0.03
      ARROW  = 0.0075
      CHEI   = .02
      OFFSEL = .04
*
      FLEXE  = .FALSE.
*
*              Calculate length of axis
*
      AXLEN = SQRT((X1-X0)*(X1-X0)+(Y1-Y0)*(Y1-Y0))
      IF(AXLEN.EQ.0)THEN
         CALL IGERR('Length of axis is zero','IGAXIS')
         GOTO 210
      ENDIF
      IF((IOPTNN.EQ.0).OR.(IOPTI.NE.0))THEN
         AXLENN = SQRT((XX1-XX0)*(XX1-XX0)+(YY1-YY0)*(YY1-YY0))
         AXLEN0 = SQRT((XX0-X0)*(XX0-X0)+(YY0-Y0)*(YY0-Y0))
         AXLEN1 = SQRT((X1-XX1)*(X1-XX1)+(Y1-YY1)*(Y1-YY1))
         IF(AXLENN.LT.EPSIL)THEN
            IOPTNN = 1
            IOPTI  = 0
            WMIN   = RWMI
            WMAX   = RWMA
            N1A    = N1AOLD
            NN(1)  = NN1OLD
            LOOP   = NN(1)
         ENDIF
      ENDIF
*
*              Calculate cosine of angle
*
      PHI    = ATAN2((Y1-Y0),(X1-X0))
      COSFI  = COS(PHI)
      SINFI  = SIN(PHI)
      ACOSFI = ABS(COSFI)
      ASINFI = ABS(SINFI)
      IF(ACOSFI.LE.EPSIL)THEN
         ACOSFI = 0.
         COSFI  = 0.
      ENDIF
      IF(ASINFI.LE.EPSIL)THEN
         ASINFI = 0.
         SINFI  = 0.
      ENDIF
      RATIO3 = (ASINFI*RATIO1)+(ACOSFI*RATIO2)
      IF(IOPTV.NE.0)RATIO3 = RATIO2
*
*              MSIDE positive, tick marks on positive side
*              MSIDE negative, tick marks on negative side
*              MSIDE zero, tick marks on both sides
*              Default is positive except for vertical axis
*
      MSIDE=1
      IF((X0.EQ.X1).AND.(Y1.GT.Y0))MSIDE=-1
      IF(IOPTPL.EQ.1)MSIDE=1
      IF(IOPTN.EQ.1)MSIDE=-1
      IF((IOPTPL+IOPTN).EQ.2)MSIDE=0
*
      LSIDE=-MSIDE
      IF(IOPTEQ.EQ.1)LSIDE=MSIDE
      IF((IOPTPL+IOPTN).EQ.2)THEN
         LSIDE=-1
         IF(IOPTEQ.EQ.1)LSIDE=1
      ENDIF
      XLSIDE=LSIDE
      XMSIDE=MSIDE
*
*              Tick marks size
*
      SITIC=SIGN(1.0,XMSIDE)
      IF((IOPTS.EQ.1).AND.(RTMS.GT.0))THEN
         TMS=RATIO3*RTMS
         ATICK(1)=SITIC*TMS
      ELSE
         ATICK(1)=SITIC*TICK*AXLEN
      ENDIF
*
      ATICK(2)=ATICK(1)/2.
      ATICK(3)=ATICK(2)/2.
*
*              Arrow size
*
      ARROLE=ARROL*AXLEN
      ARROWI=ARROW*AXLEN
*
*              Set the side of the grid
*
      IF((X0.EQ.X1).AND.(Y1.GT.Y0))THEN
         SIWIR=-1
      ELSE
         SIWIR=1
      ENDIF
*
*              Draw the axis if needed...
*
      RLWSCS=RLWSC
      CALL ISLWSC(1.)
      IF(IOPTB.EQ.0)THEN
         XPL(1)=X0
         XPL(2)=X1
         YPL(1)=Y0
         YPL(2)=Y1
         CALL IPL(2,XPL,YPL)
      ENDIF
*
*              Draw the arrow if needed...
*
      IF(IOPTA.NE.0)THEN
         XPL(1)=X1
         XPL(4)=X1
         YPL(1)=Y1
         YPL(4)=Y1
         P=AXLEN-ARROLE
         CALL IGAXI1 (P,-ARROWI,COSFI,SINFI,X0,Y0,XPL(2),YPL(2))
         CALL IGAXI1 (P,ARROWI,COSFI,SINFI,X0,Y0,XPL(3),YPL(3))
         CALL IPL(4,XPL,YPL)
      ENDIF
      CALL ISLWSC(RLWSCS)
*
*              Now the line have the text color (ticks marks)
*
      IPLCIS=IPLCI
      CALL ISPLCI(ITXCI)
*
*              No bining
*
      IF(NDIV.EQ.0)GOTO 210
      IF(WMIN.EQ.WMAX)THEN
         CALL IGERR('WMIN=WMAX (cf. HIGZ doc.)','IGAXIS')
         GOTO 210
      ENDIF
*
*              Labels preparation:
*
*              Now determine character height
*              (if LOGICAL AXFLAG=.TRUE. then we take the same
*              size than for the previous axe, stocked in
*              CHAXSZ (COMMON /AXSIZ/).
*
      IF(.NOT.AXFLAG)THEN
         IF((IOPTH.EQ.0).OR.(RALH.LT.0)) THEN
            CHHEI=CHEI*AXLEN
         ELSE
            IF(X0.NE.X1) THEN
               CHHEI=RATIO3*RALH
            ELSE
               CHHEI=RATIO2*RALH
            ENDIF
         ENDIF
         CHAXSZ=CHHEI
      ELSE
         CHHEI=CHAXSZ
      ENDIF
      CALL ISCHH (CHHEI)
*
*              Now determine the labels orientation in case of overlaps
*              (with alphanumeric labels for horizontal axis).
*
      IF(IOPT0.EQ.0.AND.IOPTO.EQ.0.AND.IOPTY.EQ.0)THEN
         IF(IOPTT.NE.0.AND.YMIN.EQ.YMAX)THEN
            BINWDH = 0.9*(AXLEN/FLOAT(N1A))
            TEXTW  = 0.
            DO 10 I=1,NHILAB
               CALL IGTEXT(0.,0.,HILABS(I),CHHEI,TEXTW,'S')
               IF(TEXTW.GT.BINWDH)THEN
                  IOPT0 = 1
                  IOPTR = 1
                  IOPTC = 0
                  IOPTL = 0
                  GOTO 20
               ENDIF
   10       CONTINUE
         ENDIF
      ENDIF
*
*              Now determine orientation of labels on axis
*
   20 IF(COSFI.GT.0.)THEN
         CALL ISCHUP(-SINFI,COSFI)
      ELSE
         CALL ISCHUP(SINFI,-COSFI)
      ENDIF
      IF(X0.EQ.X1)  CALL ISCHUP(0.,1.)
      IF(IOPTV.NE.0)CALL ISCHUP(0.,1.)
      IF(IOPTP.NE.0)CALL ISCHUP(-SINFI,COSFI)
      IF(IOPTO.NE.0)CALL ISCHUP(COSFI,SINFI)
      IF(IOPT0.NE.0)CALL IGSET('TANG',90.)
*
*              Now determine text alignment
*
      IALX=2
      IALY=0
      IF(X0.EQ.X1)IALX=3
      IF(Y0.NE.Y1)IALY=3
      IF(IOPTC.EQ.1)IALX=2
      IF(IOPTR.EQ.1)IALX=3
      IF(IOPTL.EQ.1)IALX=1
      CALL ISTXAL(IALX,IALY)
*
*              Position of labels in Y
*
      SILAB=SIGN(1.0,XLSIDE)
      IF((IOPTD.EQ.1).AND.(RALD.GE.0))THEN
         ALD=RATIO3*RALD
         YLAB=SILAB*ALD
      ELSE
         IF(IOPTG.EQ.0)THEN
            YLAB=SILAB*OFFSEL*AXLEN
         ELSE
            YLAB=SILAB*OFFSEL*AXLEN*1.3
         ENDIF
      ENDIF
*
*              Draw the linear tick marks if needed...
*
      IF(IOPTG.EQ.0)THEN
         IF(NDIV.NE.0)THEN
            IF((IOPTNN.NE.0).AND.(IOPTI.EQ.0))THEN
               DXTICK=AXLEN/(LOOP-1.)
            ELSE
               DXTICK=AXLENN/(LOOP-1.)
            ENDIF
            DO 30 K=0,LOOP-1
               L=3
               IF(MOD(K,NN(3)).EQ.0)L=2
               IF(MOD(K,NN(2)).EQ.0)L=1
               XTICK=K*DXTICK
               YTICK=0.
               IF(MSIDE.EQ.0)YTICK=YTICK-ATICK(L)
               IF((IOPTNN.NE.0).AND.(IOPTI.EQ.0))THEN
                  CALL IGAXI1(XTICK,YTICK,COSFI,SINFI,X0,Y0
     +                        ,XPL(2),YPL(2))
                  CALL IGAXI1(XTICK,ATICK(L),COSFI,SINFI,X0,Y0
     +                        ,XPL(1),YPL(1))
               ELSE
                  CALL IGAXI1(XTICK,YTICK,COSFI,SINFI,XX0,YY0
     +                        ,XPL(2),YPL(2))
                  CALL IGAXI1(XTICK,ATICK(L),COSFI,SINFI,XX0,YY0
     +                        ,XPL(1),YPL(1))
               ENDIF
               IF(IOPTV.NE.0)THEN
                  IF((X0.NE.X1).AND.(Y0.NE.Y1))THEN
                     IF(MSIDE.NE.0)THEN
                        XPL(1)=XPL(2)
                        IF(COSFI.GT.0)THEN
                           YPL(1)=YPL(2)+ATICK(L)
                        ELSE
                           YPL(1)=YPL(2)-ATICK(L)
                        ENDIF
                     ELSE
                        XML=(XPL(1)+XPL(2))/2.
                        YML=(YPL(1)+YPL(2))/2.
                        XPL(1)=XML
                        XPL(2)=XML
                        YPL(1)=YML+ATICK(L)
                        YPL(2)=YML-ATICK(L)
                     ENDIF
                  ENDIF
               ENDIF
               IF(ABS(XPL(2)-X0).LT.EPSIL.AND.
     +            ABS(YPL(2)-Y0).LT.EPSIL.AND.
     +            IOPTA.NE.0)GOTO 30
               IF(ABS(XPL(2)-X1).LT.EPSIL.AND.
     +            ABS(YPL(2)-Y1).LT.EPSIL.AND.
     +            IOPTA.NE.0)GOTO 30
               CALL IPL(2,XPL,YPL)
*
               IF(IOPTW.NE.0)THEN
                  IF(L.EQ.1)THEN
                     IF((IOPTNN.NE.0).AND.(IOPTI.EQ.0))THEN
                        CALL IGAXI1(XTICK,0.,COSFI
     +                              ,SINFI,X0,Y0
     +                              ,XPL(2),YPL(2))
                        CALL IGAXI1(XTICK,SIWIR*GRILEN
     +                              ,COSFI,SINFI,X0,Y0
     +                              ,XPL(1),YPL(1))
                     ELSE
                        CALL IGAXI1(XTICK,0.,COSFI
     +                              ,SINFI,XX0,YY0
     +                              ,XPL(2),YPL(2))
                        CALL IGAXI1(XTICK,SIWIR*GRILEN
     +                              ,COSFI,SINFI,XX0,YY0
     +                              ,XPL(1),YPL(1))
                     ENDIF
                     CALL ISLN(IGRITY)
                     CALL IPL(2,XPL,YPL)
                     CALL ISLN(1)
                  ENDIF
               ENDIF
*
   30       CONTINUE
*
            XTICK0=0.
            XTICK1=XTICK
*
            IF(((IOPTNN.EQ.0).OR.(IOPTI.NE.0)).AND.(AXLEN0.NE.0))THEN
               LOOP0=INT(AXLEN0/DXTICK)
               DO 40 K=0,LOOP0
                  L=3
                  IF(MOD(K,NN(3)).EQ.0)L=2
                  IF(MOD(K,NN(2)).EQ.0)L=1
                  YTICK0=0.
                  IF(MSIDE.EQ.0)YTICK0=YTICK0-ATICK(L)
                  CALL IGAXI1(XTICK0,YTICK0,COSFI,SINFI,XX0,YY0
     +                        ,XPL(2),YPL(2))
                  CALL IGAXI1(XTICK0,ATICK(L),COSFI,SINFI,XX0,YY0
     +                        ,XPL(1),YPL(1))
                  IF(IOPTV.NE.0)THEN
                     IF((X0.NE.X1).AND.(Y0.NE.Y1))THEN
                        IF(MSIDE.NE.0)THEN
                           XPL(1)=XPL(2)
                           IF(COSFI.GT.0)THEN
                              YPL(1)=YPL(2)+ATICK(L)
                           ELSE
                              YPL(1)=YPL(2)-ATICK(L)
                           ENDIF
                        ELSE
                           XML=(XPL(1)+XPL(2))/2.
                           YML=(YPL(1)+YPL(2))/2.
                           XPL(1)=XML
                           XPL(2)=XML
                           YPL(1)=YML+ATICK(L)
                           YPL(2)=YML-ATICK(L)
                        ENDIF
                     ENDIF
                  ENDIF
                  CALL IPL(2,XPL,YPL)
*
                  IF(IOPTW.NE.0)THEN
                     IF(L.EQ.1)THEN
                        CALL IGAXI1(XTICK0,0.
     +                             ,COSFI,SINFI,XX0,YY0
     +                             ,XPL(2),YPL(2))
                        CALL IGAXI1(XTICK0,SIWIR*GRILEN
     +                             ,COSFI,SINFI,XX0,YY0
     +                             ,XPL(1),YPL(1))
                        CALL ISLN(IGRITY)
                        CALL IPL(2,XPL,YPL)
                        CALL ISLN(1)
                     ENDIF
                  ENDIF
*
                  XTICK0=XTICK0-DXTICK
   40          CONTINUE
            ENDIF
*
            IF(((IOPTNN.EQ.0).OR.(IOPTI.NE.0)).AND.(AXLEN1.NE.0))THEN
               LOOP1=INT(AXLEN1/DXTICK)
               DO 50 K=0,LOOP1
                  L=3
                  IF(MOD(K,NN(3)).EQ.0)L=2
                  IF(MOD(K,NN(2)).EQ.0)L=1
                  YTICK1=0.
                  IF(MSIDE.EQ.0)YTICK1=YTICK1-ATICK(L)
                  CALL IGAXI1(XTICK1,YTICK1,COSFI,SINFI,XX0,YY0
     +                        ,XPL(2),YPL(2))
                  CALL IGAXI1(XTICK1,ATICK(L),COSFI,SINFI,XX0,YY0
     +                        ,XPL(1),YPL(1))
                  IF(IOPTV.NE.0)THEN
                     IF((X0.NE.X1).AND.(Y0.NE.Y1))THEN
                        IF(MSIDE.NE.0)THEN
                           XPL(1)=XPL(2)
                           IF(COSFI.GT.0)THEN
                              YPL(1)=YPL(2)+ATICK(L)
                           ELSE
                              YPL(1)=YPL(2)-ATICK(L)
                           ENDIF
                        ELSE
                           XML=(XPL(1)+XPL(2))/2.
                           YML=(YPL(1)+YPL(2))/2.
                           XPL(1)=XML
                           XPL(2)=XML
                           YPL(1)=YML+ATICK(L)
                           YPL(2)=YML-ATICK(L)
                        ENDIF
                     ENDIF
                  ENDIF
                  CALL IPL(2,XPL,YPL)
*
                  IF(IOPTW.NE.0)THEN
                     IF(L.EQ.1)THEN
                        CALL IGAXI1(XTICK1,0.
     +                             ,COSFI,SINFI,XX0,YY0
     +                             ,XPL(2),YPL(2))
                        CALL IGAXI1(XTICK1,SIWIR*GRILEN
     +                             ,COSFI,SINFI,XX0,YY0
     +                             ,XPL(1),YPL(1))
                        CALL ISLN(IGRITY)
                        CALL IPL(2,XPL,YPL)
                        CALL ISLN(1)
                     ENDIF
                  ENDIF
*
                  XTICK1=XTICK1+DXTICK
   50          CONTINUE
            ENDIF
         ENDIF
      ENDIF
*
*              Draw the numeric labels if needed...
*
      IF(IOPTU.EQ.0)THEN
         IF(IOPTG.EQ.0)THEN
            IF(N1A.NE.0)THEN
*
*              Spacing of labels
*
               IF((WMIN.EQ.WMAX).OR.(NDIV.EQ.0))THEN
                  CALL IGERR('WMIN=WMAX or NDIV=0 (cf. HIGZ doc.)'
     +            ,'IGAXIS')
                  GOTO 210
               ENDIF
               WLAB=WMIN
               DWLAB=(WMAX-WMIN)/FLOAT(N1A)
               IF((IOPTNN.NE.0).AND.(IOPTI.EQ.0))THEN
                  DXLAB=AXLEN/(N1A)
               ELSE
                  DXLAB=AXLENN/(N1A)
               ENDIF
*
*              Here we have to decide what format to generate
*              (for numeric labels only)
*
               IF(IOPTT.EQ.0)THEN
*
*              Test the magnitude, decide format
*
                  FLEXE=.FALSE.
                  NEXE=0
                  FLEXPO=.FALSE.
                  FLEXNE=.FALSE.
                  WW=MAX(ABS(WMIN),ABS(WMAX))
*
*              First case : (WMAX-WMIN)/N1A less than 0.001
*              (0.001 precision of 5 (NCAR) characters). Then we use x 10 n
*              format. If AF >=0 x10 n cannot be used
*
                  IF((ABS(WMAX-WMIN)/N1A).LT.0.00099)THEN
                     AF    = LOG10(WW)+EPSIL
                     IF(AF.GE.0.)GOTO 60
                     FLEXE = .TRUE.
                     NEXE  = INT(AF)
                     IEXE  = IABS(NEXE)
                     WLAB  = WLAB*(10.**IEXE)
                     DWLAB = DWLAB*(10.**IEXE)
                     IF1   = NCAR
                     IF2   = NCAR-2
                     GOTO 110
                  ENDIF
*
   60             IF(WW.GE.1.)THEN
                     AF=LOG10(WW)
                  ELSE
                     AF=LOG10(WW*0.0001)
                  ENDIF
                  AF=AF+EPSIL
                  NF=AF+1.0
                  IF(NF.GT.NCAR)FLEXPO=.TRUE.
                  IF(NF.LT.(-NCAR))FLEXNE=.TRUE.
*
*              Use x 10 n format.
*
                  IF(FLEXPO)THEN
                     FLEXE=.TRUE.
   70                IF(WW.GT.(10**(NCAR-1)))THEN
                        NEXE=NEXE+1
                        WW=WW/10.
                        WLAB=WLAB/10.
                        DWLAB=DWLAB/10.
                        GOTO 70
                     ENDIF
                  ENDIF
*
                  IF(FLEXNE)THEN
                     FLEXE=.TRUE.
                     RNE=1./10.**(NCAR-2)
   80                IF(WW.LT.RNE)THEN
                        NEXE=NEXE-1
                        WW=WW*10.
                        WLAB=WLAB*10.
                        DWLAB=DWLAB*10.
                        GOTO 80
                     ENDIF
                  ENDIF
*
                  NA=0
                  DO 90  I=NCAR-1,1,-1
                     ECAR=10**I
                     IF(ABS(WW).LT.ECAR)NA=NCAR-I
   90             CONTINUE
                  NDYN=N1A
  100             IF(NDYN.NE.0)THEN
                     IF((ABS((WMAX-WMIN)/NDYN).LE.0.999).AND.
     +                  (NA.LT.(NCAR-2)))THEN
                        NA=NA+1
                        NDYN=NDYN/10
                        GOTO 100
                     ENDIF
                  ENDIF
*
*              FORTRAN 77 internal write
*
                  IF2 = NA
                  IF1 = MAX(NF+NA,NCAR)+1
*
  110             IF(MIN(WMIN,WMAX).LT.0.)IF1 = IF1+1
                  IF1 = MIN(IF1,32)
*
* In some cases, IF1 and IF2 are too small....
*
  120             IF(DWLAB.lt.10.**FLOAT(-IF2))THEN
                     IF1 = IF1+1
                     IF2 = IF2+1
                     GOTO 120
                  ENDIF
*
                  WRITE (CODED,'(2H(F,I2,1H.,I2,1H))') IF1,IF2
*
               ENDIF
*
*              Here we draw labels
*
               IF(IOPTM.NE.0)THEN
                  ILOOP=N1A-1
               ELSE
                  ILOOP=N1A
               ENDIF
               DO 150 K=0,ILOOP
                  XLAB=DXLAB*K
                  IF(IOPTM.NE.0)THEN
                     XLAB=XLAB+(DXLAB/2.)
                  ENDIF
*
                  IF(IOPTT.EQ.0)THEN
                     WRITE(LABEL,CODED)WLAB
                     WLAB=WLAB+DWLAB
*
*              Here we eliminate the blanks around the label.
*
                     CALL IGAXI2(LABEL,ISTA,IEND)
*
*              Here we check if '.' is preceded by a number.
*
                     CHTEMP=' '
                     IF(LABEL(ISTA:ISTA).EQ.'.')
     +                  CHTEMP='0'//LABEL(ISTA:IEND)
                     IF(LABEL(ISTA:ISTA+1).EQ.'-.')
     +                  CHTEMP='-0'//LABEL(ISTA+1:IEND)
                     IF(CHTEMP.NE.' ')THEN
                        LABEL=CHTEMP
                        ISTA=1
                        IEND=LENOCC(LABEL)
                     ENDIF
*
*              Here we eliminate the non significiant 0 after '.'
*
  130                IF(LABEL(IEND:IEND).EQ.'0')THEN
                        IEND=IEND-1
                        GOTO 130
                     ENDIF
*
*              Here we eliminate the dot, unless dot is forced.
*
                     IF(LABEL(IEND:IEND).EQ.'.')THEN
                        IF(IOPTDO.EQ.0)IEND=IEND-1
                     ENDIF
                  ENDIF
*
*              Here we generate labels (numeric or alphanumeric).
*
                  IF((IOPTNN.NE.0).AND.(IOPTI.EQ.0))THEN
                     CALL IGAXI1 (XLAB,YLAB,COSFI,SINFI,X0,Y0,XX,YY)
                  ELSE
                     CALL IGAXI1 (XLAB,YLAB,COSFI,SINFI,XX0,YY0,XX,YY)
                  ENDIF
                  IF((Y0.EQ.Y1).AND.(IOPTO.EQ.0).AND.(IOPT0.EQ.0))THEN
                     IF(LSIDE.LT.0)THEN
                        YY=YY-CHHEI
                     ENDIF
                  ENDIF
                  IF(IOPTV.NE.0)THEN
                     IF((X0.NE.X1).AND.(Y0.NE.Y1))THEN
                        IF((IOPTNN.NE.0).AND.(IOPTI.EQ.0))THEN
                           CALL IGAXI1 (XLAB,0.,COSFI,SINFI,X0,Y0,XX,YY)
                        ELSE
                           CALL IGAXI1 (XLAB,0.,COSFI,SINFI,XX0,YY0,XX,
     +                     YY)
                        ENDIF
                        IF(COSFI.GT.0)THEN
                           YY=YY+YLAB
                           XX=XX+((FLOAT(IEND-ISTA+1)*RCHH)/2)*SINFI
                        ELSE
                           YY=YY-YLAB
                           XX=XX-((FLOAT(IEND-ISTA+1)*RCHH)/2)*SINFI
                        ENDIF
                     ENDIF
                  ENDIF
                  IF((IOPTY.EQ.0).OR.(X0.EQ.X1))THEN
                     IF(IOPTT.EQ.0)THEN
                        IF(ISTA.GT.IEND)THEN
                           CHTEMP=' '
                        ELSE
                           CHTEMP=LABEL(ISTA:IEND)
                        ENDIF
                        CALL ITX(XX,YY,CHTEMP)
                     ELSE
                        IF(K+1.GT.NHILAB)THEN
                           CHTEMP=' '
                        ELSE
                           CHTEMP=HILABS(K+1)
                        ENDIF
                        CALL ITX(XX,YY,CHTEMP)
                     ENDIF
                  ELSE
*
*              Text alignment is down
*
                     IF(IOPTT.EQ.0)THEN
                        LNLEN=IEND-ISTA+1
                     ELSE
                        IF(K+1.GT.NHILAB)THEN
                           LNLEN=0
                        ELSE
                           LNLEN=LENOCC(HILABS(K+1))
                        ENDIF
                     ENDIF
                     DO 140 L=1,LNLEN
                        IF(IOPTT.EQ.0)THEN
                           CHTEMP=LABEL(ISTA+L-1:ISTA+L-1)
                        ELSE
                           IF(LNLEN.EQ.0)THEN
                              CHTEMP=' '
                           ELSE
                              CHTEMP=HILABS(K+1)(L:L)
                           ENDIF
                        ENDIF
                        CALL ITX(XX,YY,CHTEMP)
                        YY=YY-(CHHEI*1.3)
  140                CONTINUE
                  ENDIF
  150          CONTINUE
*
*                Here we use the format x 10 ** n
*
               IF(FLEXE.AND.IOPTT.EQ.0.AND.NEXE.NE.0) THEN
                  WRITE(LABEL,'(I3)')NEXE
                  IF(X0.NE.X1)THEN
                     XFCT=XLAB
                     YFCT=YLAB-3*CHHEI
                  ELSE
                     XFCT=XLAB+3*CHHEI
                     YFCT=YLAB
                  ENDIF
                  CALL IGAXI2(LABEL,ISTA,IEND)
                  DEXPY=CHHEI
                  IF((NEXE.LE.0).OR.(Y0.EQ.Y1))THEN
                     DEXPX=2*CHHEI
                  ELSE
                     DEXPX=CHHEI
                  ENDIF
                  IF(COSFI.GT.0)THEN
                     XEXP=XFCT+DEXPX
                     YEXP=YFCT+DEXPY
                  ELSE
                     XEXP=XFCT-DEXPX
                     YEXP=YFCT-DEXPY
                  ENDIF
                  IF(X0.EQ.X1)THEN
                     IF(Y0.LT.Y1)THEN
                        XEXP=XFCT+0.5*DEXPX
                        YEXP=YFCT-DEXPY
                     ELSE
                        XEXP=XFCT-DEXPX
                        YEXP=YFCT+DEXPY
                     ENDIF
                  ENDIF
                  IF((IOPTNN.NE.0).AND.(IOPTI.EQ.0))THEN
                     CALL IGAXI1 (XFCT,YFCT,COSFI,SINFI,X0,Y0,XX,YY)
                     CALL IGAXI1 (XEXP,YEXP,COSFI,SINFI,X0,Y0 ,XEXPT,
     +               YEXPT)
                  ELSE
                     CALL IGAXI1 (XFCT,YFCT,COSFI,SINFI,XX0,YY0,XX,YY)
                     CALL IGAXI1 (XEXP,YEXP,COSFI,SINFI,XX0,YY0 ,XEXPT,
     +               YEXPT)
                  ENDIF
                  IF(NEXE.NE.1)THEN
                     XX = XX-(IEND-ISTA)*(CHHEI*0.5)
                     CALL ITX(XX,YY,'x 10')
                     CALL ISCHH (CHHEI*0.8)
                     CHTEMP=LABEL(ISTA:IEND)
                     IF(X0.EQ.X1.AND.IOPTL.NE.0)XEXPT=XX+3.5*CHHEI
                     CALL ITX(XEXPT,YEXPT,CHTEMP)
                     CALL ISCHH (CHHEI)
                  ELSE
                     CALL ITX(XX,YY,'x 10')
                  ENDIF
               ENDIF
*
            ENDIF
         ENDIF
      ENDIF
*
*              Log axis
*
      IF(IOPTG.NE.0)THEN
         IF(NDIV.NE.0)THEN
            IF((WMIN.EQ.WMAX).OR.(NDIV.EQ.0)) THEN
               CALL IGERR('WMIN=WMAX or NDIV=0 (cf. HIGZ doc.)'
     +                   ,'IGAXIS')
               GOTO 210
            ENDIF
            IF(WMIN.LE.0.)  THEN
               CALL IGERR('Negative logarithmic axis','IGAXIS')
               GOTO 210
            ENDIF
            IF(WMAX.LE.0.)    THEN
               CALL IGERR('Negative logarithmic axis','IGAXIS')
               GOTO 210
            ENDIF
            XMNLOG = LOG10(WMIN)
            IF(XMNLOG.GT.0.)THEN
               XMNLOG = XMNLOG+1.E-6
            ELSE
               XMNLOG = XMNLOG-1.E-6
            ENDIF
            X00    = 0.
            X11    = AXLEN
            H2     = LOG10(WMAX)
            H2SAV  = H2
            IF(H2.GT.0.)THEN
               H2 = H2+1.E-6
            ELSE
               H2 = H2-1.E-6
            ENDIF
            IH1    = IFIX(XMNLOG)
            IH2    = 1+IFIX(H2)
            NBININ = IH2-IH1+1
            AXMUL  = (X11-X00)/(H2SAV-XMNLOG)
*
*              If:
*                  a) The number of decades is less than two.
*                           and
*                  b) 1 =< WMIN and WMAX =<10000
*                           and
*                  c) There is no labels overlap.
*              then some intermediate label are drawn (INTLOG=.TRUE).
*
            INTLOG = .FALSE.
            IF(LOG10(WMAX/WMIN).LT.2..AND.
     +         WMIN.GE.0.01.AND.WMAX.LE.10000.)INTLOG = .TRUE.
            SMALD = (LOG10(1./0.9)/LOG10(WMAX/WMIN))*AXLEN
            IF(XMIN.EQ.XMAX.AND.SMALD.LE.CHHEI)INTLOG = .FALSE.
            IF(YMIN.EQ.YMAX)THEN
               TEXTW  = 0.
               CALL IGTEXT(0.,0.,'0.01',CHHEI,TEXTW,'S')
               IF(0.5*TEXTW.GT.SMALD)INTLOG = .FALSE.
            ENDIF
*
*              Plot decade and intermediate tick marks
*
            I    = IH1-2
            NLAB = IH1
            IF ((XMNLOG.GT.0.).AND.
     +         ((XMNLOG-FLOAT(IH1)).GT.0.)) NLAB = NLAB+1
            DO 190 J=1,NBININ
*
*              Plot decade
*
               I = I+1
               R = FLOAT(I)
               IF(Y0.EQ.Y1.AND.J.EQ.1)YLAB=YLAB-CHHEI*0.65
               XONE=X00+AXMUL*(R-XMNLOG)
               IF(X00.GT.XONE) GOTO 160
               IF(XONE.GT.X11) GOTO 200
               XTWO=XONE
               Y=0.
               IF(MSIDE.EQ.0)Y=Y-ATICK(1)
               CALL IGAXI1(XONE,Y,COSFI,SINFI,X0,Y0,XPL(2),YPL(2))
               CALL IGAXI1(XTWO,ATICK(1),COSFI,SINFI,X0,Y0
     +                     ,XPL(1),YPL(1))
               IF(IOPTV.NE.0)THEN
                  IF((X0.NE.X1).AND.(Y0.NE.Y1))THEN
                     IF(MSIDE.NE.0)THEN
                        XPL(1)=XPL(2)
                        IF(COSFI.GT.0)THEN
                           YPL(1)=YPL(2)+ATICK(1)
                        ELSE
                           YPL(1)=YPL(2)-ATICK(1)
                        ENDIF
                     ELSE
                        XML=(XPL(1)+XPL(2))/2.
                        YML=(YPL(1)+YPL(2))/2.
                        XPL(1)=XML
                        XPL(2)=XML
                        YPL(1)=YML+ATICK(1)
                        YPL(2)=YML-ATICK(1)
                     ENDIF
                  ENDIF
               ENDIF
               CALL IPL(2,XPL,YPL)
*
               IF(IOPTW.NE.0)THEN
                  CALL IGAXI1(XONE,0.,COSFI,SINFI,X0,Y0,XPL(2),YPL(2))
                  CALL IGAXI1(XONE,SIWIR*GRILEN,COSFI,SINFI,X0,Y0
     +                        ,XPL(1),YPL(1))
                  CALL ISLN(IGRITY)
                  CALL IPL(2,XPL,YPL)
                  CALL ISLN(1)
               ENDIF
*
               IF(IOPTU.EQ.0) THEN
*
*              Here we generate labels (numeric only).
*
                  IF(INTLOG)THEN
                     RLAB = 10.**FLOAT(NLAB)
                     CALL IZRTOC(RLAB,LABEL)
                     CALL IGAXI2(LABEL,ISTA,IEND)
                     IF(LABEL(IEND:IEND).EQ.'.')IEND = IEND-1
                  ELSE
                     WRITE(LABEL,'(I3)')NLAB
                     CALL IGAXI2(LABEL,ISTA,IEND)
                  ENDIF
                  DEXPY=CHHEI
                  IF(NLAB.GT.0)THEN
                     DEXPX=1.25*CHHEI
                  ELSE
                     DEXPX=2.25*CHHEI
                  ENDIF
                  IF(COSFI.GT.0)THEN
                     XEXP=XONE+DEXPX
                     YEXP=YLAB+DEXPY
                  ELSE
                     XEXP=XONE-DEXPX
                     YEXP=YLAB-DEXPY
                  ENDIF
                  IF(Y0.EQ.Y1.AND.NLAB.LT.0)XEXP=XONE+0.5*DEXPX
                  IF(X0.EQ.X1)THEN
                     IF(Y0.LT.Y1)THEN
                        XEXP=XONE+0.5*DEXPX
                        YEXP=YLAB-DEXPY
                     ELSE
                        XEXP=XONE-DEXPX
                        YEXP=YLAB+DEXPY
                     ENDIF
                  ENDIF
                  CALL IGAXI1 (XONE,YLAB,COSFI,SINFI,X0,Y0,XX,YY)
                  CALL IGAXI1 (XEXP,YEXP,COSFI,SINFI,X0,Y0,XEXPT,YEXPT)
                  IF((Y0.EQ.Y1).AND.(IOPTO.EQ.0).AND.(IOPT0.EQ.0))THEN
                     IF(LSIDE.LT.0)THEN
                        YY=YY-CHHEI
                        YEXPT=YEXPT-CHHEI
                     ENDIF
                  ENDIF
                  IF(IOPTV.NE.0)THEN
                     IF((X0.NE.X1).AND.(Y0.NE.Y1))THEN
                        XEXPT=XX+0.5*DEXPX
                        YEXPT=YY+DEXPY
                     ENDIF
                  ENDIF
                  IF(X0.EQ.X1.AND.IOPTL.NE.0)XEXPT=XX+2.*CHHEI
                  IF(N1A.EQ.0)GOTO 210
                  KMOD=NBININ/N1A
                  IF(KMOD.EQ.0)KMOD=1000000
                  IF((NBININ.LE.N1A)
     +                    .OR.(J.EQ.1)
     +                    .OR.(J.EQ.NBININ)
     +                    .OR.((NBININ.GT.N1A)
     +                    .AND.(MOD(J,KMOD).EQ.0)))THEN
                     IF((NLAB.NE.0).AND.(NLAB.NE.1))THEN
                        IF(INTLOG)THEN
                           CHTEMP=LABEL(ISTA:IEND)
                           CALL ITX(XX,YY,CHTEMP)
                        ELSE
                           XX = XX-(IEND-ISTA)*(CHHEI*0.5)
                           CALL ITX(XX,YY,'10')
                           CALL ISCHH (CHHEI*0.8)
                           CHTEMP=LABEL(ISTA:IEND)
                           CALL ITX(XEXPT,YEXPT,CHTEMP)
                           CALL ISCHH (CHHEI)
                        ENDIF
                     ENDIF
                     IF(NLAB.EQ.0)CALL ITX(XX,YY,'1')
                     IF(NLAB.EQ.1)CALL ITX(XX,YY,'10')
                  ENDIF
                  NLAB=NLAB+1
               ENDIF
  160          CONTINUE
*
               DO 180 K=2,9
*
*              Plot intermediate tick marks
*
                  XONE=X00+AXMUL*(LOG10(FLOAT(K))+FLOAT(I)-XMNLOG)
                  IF(X00.GT.XONE) GOTO 180
                  IF(XONE.GT.X11) GOTO 200
                  Y=0.
                  IF(MSIDE .NE. 0) GOTO 170
                  Y=Y-ATICK(2)
  170             CONTINUE
                  XTWO=XONE
                  CALL IGAXI1(XONE,Y,COSFI,SINFI
     +                       ,X0,Y0,XPL(2),YPL(2))
                  CALL IGAXI1(XTWO,ATICK(2),COSFI,SINFI,X0,Y0
     +                       ,XPL(1),YPL(1))
                  IF(IOPTV.NE.0)THEN
                     IF((X0.NE.X1).AND.(Y0.NE.Y1))THEN
                        IF(MSIDE.NE.0)THEN
                           XPL(1)=XPL(2)
                           IF(COSFI.GT.0)THEN
                              YPL(1)=YPL(2)+ATICK(2)
                           ELSE
                              YPL(1)=YPL(2)-ATICK(2)
                           ENDIF
                        ELSE
                           XML=(XPL(1)+XPL(2))/2.
                           YML=(YPL(1)+YPL(2))/2.
                           XPL(1)=XML
                           XPL(2)=XML
                           YPL(1)=YML+ATICK(2)
                           YPL(2)=YML-ATICK(2)
                        ENDIF
                     ENDIF
                  ENDIF
                  IDN=N1A*2
                  IF((NBININ.LE.IDN).OR.
     +              ((NBININ.GT.IDN).AND.(K.EQ.5)))THEN
                     CALL IPL(2,XPL,YPL)
*
* Draw the intermediate LOG labels if requested
*
                     IF(INTLOG.AND.IOPTU.EQ.0)THEN
                        RLAB=FLOAT(K)*(10.**FLOAT(NLAB-1))
                        CALL IZRTOC(RLAB,CHTEMP)
                        LNLEN=LENOCC(CHTEMP)
                        IF(CHTEMP(LNLEN:LNLEN).EQ.'.')LNLEN=LNLEN-1
                        CALL IGAXI1 (XONE,YLAB,COSFI,SINFI,X0,Y0,XX,YY)
CCC                        IF((X0.EQ.X1).AND.(IOPTP.EQ.0))THEN
CCC                           IF(LSIDE.LT.0)THEN
CCC                              IF(NLAB.EQ.0)THEN
CCC                                 NCH=1
CCC                              ELSE
CCC                                 NCH=2
CCC                              ENDIF
CCC                              XX=XX+NCH*CHHEI
CCC                           ENDIF
CCC                        ENDIF
                        IF((Y0.EQ.Y1).AND.(IOPTO.EQ.0)
     +                               .AND.(IOPT0.EQ.0))THEN
                           IF(LSIDE.LT.0)THEN
                              YY=YY-CHHEI
                           ENDIF
                        ENDIF
                        IF(IOPTV.NE.0)THEN
                           IF((X0.NE.X1).AND.(Y0.NE.Y1))THEN
                              CALL IGAXI1(XONE,YLAB,COSFI,SINFI,X0,Y0
     +,                                   XX,YY)
                              IF(COSFI.GT.0)THEN
                                 YY=YY+YLAB
                              ELSE
                                 YY=YY-YLAB
                              ENDIF
                           ENDIF
                        ENDIF
                        CALL ITX(XX,YY,CHTEMP(1:LNLEN))
                     ENDIF
*
* Draw the intermediate LOG grid if only three decades are requested
*
                     IF(IOPTW.NE.0.AND.NBININ.LE.5)THEN
                        CALL IGAXI1(XONE,0.,COSFI,SINFI,X0,Y0,XPL(2)
     +,                             YPL(2))
                        CALL IGAXI1(XONE,SIWIR*GRILEN,COSFI,SINFI,X0,Y0
     +,                             XPL(1),YPL(1))
                        CALL ISLN(IGRITY)
                        CALL IPL(2,XPL,YPL)
                        CALL ISLN(1)
                     ENDIF
*
                  ENDIF
  180          CONTINUE
  190       CONTINUE
  200       CONTINUE
         ENDIF
      ENDIF
*
  210 CONTINUE
      CALL ISELNT(ICTR)
      CALL ISCHH(CHSIZ)
      CALL ISCHUP(CHUPXV,CHUPYV)
      CALL ISTXAL(IALH,IALV)
      CALL ISLN(ILNV)
      CALL ISPLCI(IPLCIS)
      CALL ISCLIP(ICLIPV)
  220 ZFLAG=ZFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
