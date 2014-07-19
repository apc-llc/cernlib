*
* $Id: hplot.F,v 1.1.1.1 1996/01/19 10:50:10 mclareni Exp $
*
* $Log: hplot.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:10  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/04 04/10/95  17.30.08  by  O.Couet
*-- Author :
      SUBROUTINE HPLOT(IDDD,CHOPT,KICASE,NUMM)
*.==========>
*.
*.                     HPLOT basic routine
*.
*.   Input parameter:
*.
*. IDDD                : Histogram IDentifier.
*.
*. CHARACTER*(*) CHOPT :
*.
*. CHOPT = 'S'         : (or 'SAME' for compatibilty) histogram is
*.                       plotted in the same "ZONE" as the last one.
*. CHOPT = 'K'         : option "Keep" (keeping the histogram ZEBRA bank
*.                       in memory for later update (option "U") of the
*.                       same histogram or later addition
*.                       (or contribution : option "+") of other
*.                       histograms).
*. CHOPT = 'U'         : Update of the histogram (which must have been
*.                       plot the first time with option "K" if
*.                       windowing option is selected).
*. CHOPT = '+'         : Addition (or contribution) of histogram IDDD
*.                       upon the last histogram plotted or the last
*.                       created with option 'K' if windowing option is
*.                       selected.
*. CHOPT = '-'         : Same as '+' but the contains of the histogram
*.                       is substract.
*. CHOPT = '+-'        : Draw the for each bin delta between
*.                       2 histograms
*. CHOPT = 'A'         : If specified, axis are not drawn
*. CHOPT = 'BOX'       : Draw 2D plot with proportionnal Boxes
*. CHOPT = 'ARR'       : Draw 2D plot with Arrows
*. CHOPT = 'COL'       : Draw 2D plot with Colors
*. CHOPT = 'LEGO'      : Draw 2D plot as a Lego plot
*. CHOPT = 'LEGO1'     : Draw 2D plot as a Lego (mode 1 see IGTABL)
*. CHOPT = 'LEGO2'     : Draw 2D plot as a Lego (mode 2 see IGTABL)
*. CHOPT = 'SURF'      : Draw 2D plot as a Surface
*. CHOPT = 'SURF1'     : Draw 2D plot as a Surface (mode 1 see IGTABL)
*. CHOPT = 'SURF2'     : Draw 2D plot as a Surface (mode 2 see IGTABL)
*. CHOPT = 'CONT'      : Draw 2D plot as a Contour plot
*. CHOPT = 'SCAT'      : Draw 2D plot a Scatter plot
*. CHOPT = 'TEXT'      : Draw 2D plot with the contains of each cell
*. CHOPT = 'CHAR'      : Draw 2D plot with a character set
*. CHOPT = 'ARR'       : Draw 2D plot with arrows
*. CHOPT = 'HIST'      : Draw only the histogram
*. CHOPT = 'FUNC'      : Draw only the function
*.                       (for example in case of fit)
*. CHOPT = 'E'         : Errors with current marker type and size
*.                       are drawn.
*. CHOPT = 'L', 'C', 'F', '*', 'M', 'B' or 'N' :
*.                      (cf. HIGZ doc., routine IGHIST)
*.
*. CHARACTER*(*) KICASE:
*.
*. KICASE = 'SLIX'     : gives slices in X
*. KICASE = 'SLIY'     : gives slices in Y
*. KICASE = 'BANX'     : gives bands in X
*. KICASE = 'BANY'     : gives bands in Y
*. KICASE = 'PROX'     : gives projection in X
*. KICASE = 'PROY'     : gives projection in Y
*. KICASE = 'HIST'     : gives the histogram (scatter plot) only.
*.
*. NUMM                : refers to slices or banks (if NUM=n: nth slice
*.                             or bank, if NUM=0: all slices or banks).
*. Remarks :
*. IDZB  = current histogram (when calling HPLOT(ID,...))
*. IDZK  = last histogram created with option "K" (if windowing)
*.         or histogram just plotted before "+" (if no windowing).
*..=========>
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl13.inc"
#include "hbook/hcflag.inc"
      CHARACTER*(*) CHOPT,KICASE
      CHARACTER*4 KICAS2
      CHARACTER*16 CHOPT2
      DIMENSION PAR(2)
*._____________________________
*
      IF(IPLNUM.LT.0)IPLNUM=0
*----
*     set up defaults
*----
      IDD    = IDDD
      IDZB   = IDD
      LHPLIP = 0
      NUM    = 0
      IRET   = 3
      KTYPE  = 1
      LOSLIC = .FALSE.
      LOSAME = .FALSE.
      LOZERO = IDD.EQ.0
      LOCM   = .FALSE.
      IERROR = 0
*----
      NARG=4
      CALL NOARG(NARG)
      IF(NARG.GT.4) NARG=4
      GOTO (50,30,20,10),NARG
*
   10 NUM=NUMM
*
   20 CALL UCTOH(KICASE,ICASE,4,4)
      KTYPE=IUCOMP(ICASE,IDENT(1),8)
      IF(KTYPE.EQ.0)KTYPE=1
*----
*     Decode CHOPT
*----
   30 CONTINUE
      CHOPT2=CHOPT
      IF(IDEF1D.GT.0.AND.CHOPT2.EQ.' ')THEN
         IF(IDEF1D.EQ.1)CHOPT2 = 'L'
         IF(IDEF1D.EQ.2)CHOPT2 = 'C'
         IF(IDEF1D.EQ.3)CHOPT2 = 'B'
         IF(IDEF1D.EQ.4)CHOPT2 = 'P'
         IF(IDEF1D.EQ.5)CHOPT2 = '*'
         IF(IDEF1D.EQ.6)CHOPT2 = 'E'
      ENDIF
      NCH=LENOCC(CHOPT2)
      IOPTH=1
      IOPT0=0
      IOPTF=1
      IF(NCH.GE.3)THEN
         IOP=INDEX(CHOPT2,'HIST')
         IF(IOP.GT.0)THEN
            CHOPT2(IOP:IOP+3)='    '
            IOPTF=0
            IOPTH=2
         ENDIF
         IOP=INDEX(CHOPT2,'FUNC')
         IF(IOP.GT.0)THEN
            CHOPT2(IOP:IOP+3)='    '
            IF(IOPTH.EQ.1)IOPTH=0
            IOPTF=2
         ENDIF
         IOP=INDEX(CHOPT2,'LEGO1')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+4)='     '
         IOP=INDEX(CHOPT2,'LEGO2')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+4)='     '
         IOP=INDEX(CHOPT2,'LEGO')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+3)='    '
         IOP=INDEX(CHOPT2,'SURF1')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+4)='     '
         IOP=INDEX(CHOPT2,'SURF2')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+4)='     '
         IOP=INDEX(CHOPT2,'SURF')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+3)='    '
         IOP=INDEX(CHOPT2,'BOX')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+2)='   '
         IOP=INDEX(CHOPT2,'ARR')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+2)='   '
         IOP=INDEX(CHOPT2,'COL')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+2)='   '
         IOP=INDEX(CHOPT2,'CONT')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+3)='    '
         IOP=INDEX(CHOPT2,'SCAT')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+3)='    '
         IOP=INDEX(CHOPT2,'TEXT')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+3)='    '
         IOP=INDEX(CHOPT2,'CHAR')
         IF(IOP.GT.0)CHOPT2(IOP:IOP+3)='    '
         IF(INDEX(CHOPT2,'0').NE.0)IOPT0=1
      ENDIF
      CALL UOPTC(CHOPT2,'SUK+EA',IOPT)
      IC=0
      DO 40 I=1,4
         IF(IOPT(I).NE.0)IC=IC+1
   40 CONTINUE
      IF(IC.GT.1)THEN
         CALL HBUG(' Select only one option "SUK+" at a time'
     +            ,'HPLOT',IDD)
         GOTO 999
      ENDIF
      IF(NCH.EQ.4)THEN
         IF(CHOPT2(1:4).EQ.'SAME')THEN
            IOPTA=0
            IOPTE=0
         ENDIF
      ENDIF
*----
*     Option '+' '-' '+-'
*----
      IF(INDEX(CHOPT2,'-').NE.0)THEN
         IF(IOPTP.NE.0)THEN
            IOPTP=2
         ELSE
            IOPTP=-1
         ENDIF
      ENDIF
*
      CALL UOPTC(CHOPT2,'LC*PBH',IHOPT)
*----
*     Option 'L','C','*' or 'M' are ignored for "live" histograms
*----
      IF((IOPTU.NE.0).OR.(IOPTK.NE.0).OR.(IOPTP.NE.0))THEN
         IF(IHOPTL.NE.0)THEN
            CALL HBUG('"L" ignored for "live" Histo.','HPLOT',IDD)
            IF((IHOPTH.EQ.0).AND.(IHOPTB.EQ.0))GOTO 999
         ENDIF
         IF(IHOPTC.NE.0)THEN
            CALL HBUG('"C" ignored for "live" Histo.','HPLOT',IDD)
            IF((IHOPTH.EQ.0).AND.(IHOPTB.EQ.0))GOTO 999
         ENDIF
         IF(IHOPTT.NE.0)THEN
            CALL HBUG('"*" ignored for "live" Histo.','HPLOT',IDD)
            IF((IHOPTH.EQ.0).AND.(IHOPTB.EQ.0))GOTO 999
         ENDIF
         IF(IHOPTP.NE.0)THEN
            CALL HBUG('"P" ignored for "live" Histo.','HPLOT',IDD)
            IF((IHOPTH.EQ.0).AND.(IHOPTB.EQ.0))GOTO 999
         ENDIF
      ENDIF
*
      IF(IOPTS.NE.0)LOSAME=.TRUE.
*
      IF(LOWIND)THEN
         IF(IOPTK.NE.0)IDZK=IDD
      ELSE
         IF((IOPTP.EQ.0).AND.(IOPTU.EQ.0))IDZK=IDD
      ENDIF
*----
*     option SAME with DMOD set to 0 (default value) :
*     automatic incrementation of line type.
*----
   50 CONTINUE
      IF(.NOT.LOSAME)ILTSAM=ILTYP
      IF((LOSAME).AND.LTYPFL)THEN
         IF(IHTYP.EQ.0)THEN
            ILTSAM=ILTSAM+1
            IF(ILTSAM.GT.5)ILTSAM=1
         ENDIF
         CALL ISLN(ILTSAM)
      ENDIF
*
   60 CALL HLOOP(IDD,'HPLOT ',IRET)
      IF(IRET.EQ.0) GOTO 120
      CALL HDCOFL
      IF(I123.EQ.0)GOTO 110
      LCONT=LQ(LCID-1)
*
*            1-DIM histogram
*
      IF(KTYPE.LT.3.AND.I1.NE.0)THEN
         LPRX=LCID+KNCX
         NOENT=I9
         CALL HPLONE(CHOPT,IERROR)
      ENDIF
*
*            2-DIM histogram
*
      IF(KTYPE.LT.3.AND.I1.EQ.0)THEN
         LCONT=LQ(LCID-1)
         LSCAT=LCONT
         IF(INDEX(CHOPT,'LEGO').NE.0.OR.INDEX(CHOPT,'SURF').NE.0)THEN
            NPAR=2
            PAR(1)=30.
            PAR(2)=30.
         ELSE
            NPAR=0
         ENDIF
         CALL HPLTAB(ID,NPAR,PAR,CHOPT)
         IF(IQ(LCONT+KNOENT).EQ.0)GOTO 110
      ENDIF
      IF(I230.EQ.0)GOTO 110
*
*             Projection X
*
      IF(KTYPE.EQ.3.OR.KTYPE.EQ.0)THEN
         LCONT=LQ(LCID-2)
         LPRX=LCID+KNCX
         IF(LCONT.NE.0)THEN
            NOENT=I9
            CALL HPLONE(CHOPT,IERROR)
         ENDIF
      ENDIF
*
*             Projection Y
*
      IF(KTYPE.EQ.4.OR.KTYPE.EQ.0)THEN
         LCONT=LQ(LCID-3)
         LPRX=LCID+KNCY
         IF(LCONT.NE.0)THEN
            NOENT=I10
            CALL HPLONE(CHOPT,IERROR)
         ENDIF
      ENDIF
*
*             Slices X
*
      IF(KTYPE.EQ.5.OR.KTYPE.EQ.0)THEN
         LSLIX=LQ(LCID-4)
         LPRX=LCID+KNCX
         IF(LSLIX.NE.0)THEN
            NOENT=I9
            IF(NUM.NE.999)THEN
               DO 70 I=1,IQ(LSLIX-2)
                  IF(NUM.NE.0.AND.NUM.NE.I)GOTO 70
                  LCONT=LQ(LSLIX-I)
                  CALL HPLONE(CHOPT,IERROR)
   70          CONTINUE
            ENDIF
         ENDIF
      ENDIF
*
*             Slices Y
*
      IF(KTYPE.EQ.6.OR.KTYPE.EQ.0)THEN
         LSLIY=LQ(LCID-5)
         LPRX=LCID+KNCY
         IF(LSLIY.NE.0)THEN
            NOENT=I10
            IF(NUM.NE.999)THEN
               DO 80 I=1,IQ(LSLIY-2)
                  IF(NUM.NE.0.AND.NUM.NE.I)GOTO 80
                  LCONT=LQ(LSLIY-I)
                  CALL HPLONE(CHOPT,IERROR)
   80          CONTINUE
            ENDIF
         ENDIF
      ENDIF
*
*              Bande X
*
      IF(KTYPE.EQ.7.OR.KTYPE.EQ.0)THEN
         LBANX=LQ(LCID-6)
         LPRX=LCID+KNCX
         IF(LBANX.NE.0)THEN
            NOENT=I9
            NBX=1
   90       IF(NUM.EQ.0.OR.NUM.EQ.NBX)THEN
               LCONT=LQ(LBANX-1)
               CALL HPLONE(CHOPT,IERROR)
            ENDIF
            NBX=NBX+1
            LBANX=LQ(LBANX)
            IF(LBANX.NE.0)GOTO 90
         ENDIF
      ENDIF
*
*             Bande Y
*
      IF(KTYPE.EQ.8.OR.KTYPE.EQ.0)THEN
         LBANY=LQ(LCID-7)
         LPRX=LCID+KNCY
         IF(LBANY.NE.0)THEN
            NOENT=I10
            NBY=1
  100       IF(NUM.EQ.0.OR.NUM.EQ.NBY)THEN
               LCONT=LQ(LBANY-1)
               CALL HPLONE(CHOPT,IERROR)
            ENDIF
            NBY=NBY+1
            LBANY=LQ(LBANY)
            IF(LBANY.NE.0)GOTO 100
         ENDIF
      ENDIF
*
*     GOTO next histogram
*
  110 IRET=2
* Case of error (ie LOG scale on Y=<0)
      IF(IERROR.NE.0)GOTO 60
* With 2D histos STAT, FILE, DATE etc... are drawn in HPLTAB
      IF(KTYPE.LT.3.AND.I1.EQ.0)GOTO 60
* Draw STAT, FILE, DATE etc...
      IF((LOTIC).AND.(.NOT.LOSAME))CALL HPLWIR(' ',BIGP,BIGP,'TICK')
      IF((LOSTAT).AND.(.NOT.LOSAME))THEN
         KICAS2=' '
         NUM2=0
         IF(NARG.GE.3)KICAS2=KICASE
         IF(NARG.GE.4)NUM2=NUMM
         CALL HPLSTA(ID,KICAS2,NUM2)
      ENDIF
      IF((I12.NE.0).AND.(LOFIT).AND.(.NOT.LOSAME))CALL HPLFIT
      CALL HPLFIL
      CALL HPLDAT
      GOTO 60
*
  120 CALL ISLN(ILTYP)
*
  999 END
