*
* $Id: hplh.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplh.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 13/04/95  11.08.54  by  O.Couet
*-- Author :
      SUBROUTINE HPLH
*.==========>
*.            Draws a histogram
*.
*.       Description of ZEBRA bank (link LHPLOT)
*.
*.    Type Address                     Explanation
*.
*.  INT IQ(LSDIR+1)              : ID    (Histogram IDentifier).
*.  INT IQ(LSDIR+2)              : NTWIN (transformation number).
*.  INT IQ(LSDIR+3)              : ILOGY ( 1 if LOGY=.TRUE.,
*.                                         0 otherwise).
*.  INT IQ(LSDIR+4)              : ILOGX ( 1 if LOGX=.TRUE.,
*.                                         0 otherwise).
*.  INT IQ(LSDIR+5)              : I6    ( 1 if non-equidistant bins,
*.                                         0 otherwise).
*.  INT IQ(LSDIR+6)              : ICMAX (number of channels of the
*.                                 whole histogram, with no zoom).
*.  INT IQ(LSDIR+7)              : First channel plotted.
*.  INT IQ(LSDIR+8)              : Last channel plotted.
*.  HOL IQ(LSDIR+9)              : CHOPT (option set when calling
*.                                 IGHIST, eg. "BN" for Non-equidistant
*.                                 Bars. CHOPT contains always less
*.                                 than 4 characters)
*.  REA Q(LSDIR+10)              : Offset (for option Bar)
*.  REA Q(LSDIR+11)              : Width  (for option Bar)
*.  REA Q(LSDIR+12)              : XLOW
*.  REA Q(LSDIR+13)              : XHIGH
*.  REA Q(LSDIR+14)              : YLOW
*.  REA Q(LSDIR+15)              : YHIGH
*.  REA Q(LSDIR+16)              : XWMINI
*.  REA Q(LSDIR+17)              : XWMAXI
*.  REA Q(LSDIR+18)              : YWMINI
*.  REA Q(LSDIR+19)              : YWMAXI
*.  REA Q(LSDIR+20)              }
*.             ...               } : Y(ICMAX)   (Y ordinate of each bin)
*.  REA Q(LSDIR+20+(ICMAX-1))    }
*.  REA Q(LSDIR+20+ICMAX)        }
*.             ...               } : X(...)     (X abcisse of each bin)
*.  REA Q(LSDIR+20+ICMAX+1)      }   X(2)       if bins are equidistants
*.            or
*.  REA Q(LSDIR+20+ICMAX+ICMAX)  }   X(ICMAX+1) if bins are
*.                                   non-equidistants.
*.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl13.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hplot/quest.inc"
      CHARACTER*4 CHOPTG,CHTMPG
      CHARACTER*16 CHOPTH
      LOGICAL LHBAR,NOKEEP
      DIMENSION IXIO(2)
      SAVE IXIO
*._____________________________
*
      CALL IGQ('LWID',RLWID)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('FASI',RFASI)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('BORD',RBORD)
*
      IFIRST           = 1
      IF(LOZOOM)IFIRST = NCMIN
      ILAST            = ICMAX
      IF(LOZOOM)ILAST  = NCMAX
      NBINS            = ILAST-IFIRST+1
*---
*     If the histogram is too big (more than 99999 channels), it is
*     not possible to keep an unpacked copy in memory. In this case,
*     the flag NOKEEP is set ON and all the code to build the keeped
*     structure in memory is skipped.
*----
      IF(ICMAX.GT.99999)THEN
         NOKEEP     = .TRUE.
         RQUEST(80) = XMINI
         RQUEST(81) = XMAXI
         IQUEST(82) = LQ(LCID-1)
         IQUEST(83) = IFIRST
         IQUEST(84) = ILAST
         GOTO 40
      ELSE
         NOKEEP     = .FALSE.
      ENDIF
*---
*     Creation of "LIFE" bank if not option "Update" or "+"
*----
      IF((IOPTU.NE.0).OR.(IOPTP.NE.0))THEN
         LSDIR=LHPLIP
         LHBAR=.FALSE.
         CALL UHTOC(IQ(LSDIR+9),4,CHOPTG,4)
         CALL UOPTC(CHOPTG,'B',IOPTG)
         IF(IOPTG.NE.0)LHBAR=.TRUE.
         IF(LHBAR)THEN
            ROFZB=Q(LSDIR+10)
            RWDZB=Q(LSDIR+11)
         ENDIF
         IFIRSO = IQ(LSDIR+7)
         ILASTO = IQ(LSDIR+8)
      ELSE
         IF(I6.NE.0)THEN
            MBMAX=22+2*ICMAX+1
         ELSE
            MBMAX=22+ICMAX+2
         ENDIF
         LSDIR = LQ(LHPLOT-1)
         IF(LSDIR.NE.0)THEN
            IF(JBIT(IQ(LSDIR),1).NE.0)THEN
               CALL MZBOOK(IHDIV,LR2,LSDIR,0,'HIST',0,0,MBMAX,IXIO,0)
               LSDIR=LR2
               GOTO 10
            ENDIF
         ENDIF
         IF(LSDIR.EQ.0)THEN
            CALL MZFORM('HIST','8I 1H -F',IXIO)
            IQUEST(1)=0
            CALL MZBOOK(IHDIV,LSDIR,LHPLOT,-1,'HIST',0,0,MBMAX,IXIO,0)
         ELSE
            IF(IQ(LSDIR-1).LT.MBMAX)THEN
               CALL MZPUSH(IHDIV,LSDIR,0,MBMAX-IQ(LSDIR-1),'I')
            ELSE
               IF(IQ(LSDIR-1).GT.200.AND.MBMAX.LT.200)THEN
                  CALL MZPUSH(IHDIV,LSDIR,0,200-IQ(LSDIR-1),'I')
               ENDIF
            ENDIF
         ENDIF
   10    IQ(LSDIR+7)=IFIRST
         IQ(LSDIR+8)=ILAST
      ENDIF
*----
*     check if there is enough space in memory to create a picture.
*     If not, the option ZFL in turned off.
*----
      IF(LOZFL)THEN
         NWNEED = 2*NBINS+100
         CALL MZNEED(IXHIGZ,NWNEED,'G')
         IF(IQUEST(11).LT.0)THEN
            IDT   = IQ(LSDIR+1)
            CALL HBUG('No space to store the picture','HPLOT',IDT)
            LOZFL = .FALSE.
            CALL IGZSET('G')
         ENDIF
      ENDIF
*----
*     index initialization of array Y and X
*----
      LIY=LSDIR+19
      LIX=LIY+ICMAX
*
      IF(((IOPTU.NE.0).OR.(IOPTP.NE.0)).AND.(IQ(LSDIR+5).EQ.0))
     +  DELTAX=(Q(LIX+2)-Q(LIX+1))/(IQ(LSDIR+8)-IQ(LSDIR+7)+1)
*----
*     loop round histogram bins
*----
      DO 20 J=IFIRST,ILAST
         LIY = LSDIR+19
         LIX = LIY+ICMAX
         C1  = HCX(J,1)*FACTOR
         IF(ABS(YMAXI-YMINI).GT.0.)THEN
            IF(.NOT.LOGYFL)THEN
               YB = C1
            ELSE
               YB = LOG10(MAX(C1,10**YMINI))
            ENDIF
         ENDIF
         YB = MAX(YB,YMINI)
         YB = MIN(YB,YMAXI)
*----
* Options '+', '-', '+-' and 'U'pdate
* '+'  IOPTP = 1
* '-'  IOPTP = -1
* '+-' IOPTP = 2
* 'U'  IOPTU = 1
*----
         IF((IOPTU.NE.0).OR.(IOPTP.NE.0))THEN
*----
* Compute Y1 Y2
*----
            Y1 = Q(LIY+J-IFIRST+IFIRSO)
            Y2 = YB
            IF(ABS(IOPTP).EQ.1)THEN
               IF(.NOT.LOGYFL)THEN
                  Y2 = Y1+IOPTP*Y2
               ELSE
                  Y2 = LOG10(10**Y1+IOPTP*10**Y2)
               ENDIF
            ENDIF
*----
* Compute YM1 YM2
*----
            IF(.NOT.LHBAR)THEN
               YADD = HCX(J,1)*FACTOR
               IF(LOGYFL)THEN
                  YCUR = 10**Q(LIY+J-IFIRST+IFIRSO)
               ELSE
                  YCUR = Q(LIY+J-IFIRST+IFIRSO)
               ENDIF
               IF(J.NE.ILAST)THEN
                  YNC = HCX(J+1,1)*FACTOR
                  IF(LOGYFL)THEN
                     YNXT = (10**Q(LIY+J+1-IFIRST+IFIRSO))
                  ELSE
                     YNXT = Q(LIY+J+1-IFIRST+IFIRSO)
                  ENDIF
               ELSE
                  YNC  = 0.
                  YNXT = 0.
               ENDIF
               IF(J.NE.IFIRST)THEN
                  YPC = HCX(J-1,1)*FACTOR
                  IF(LOGYFL)THEN
                     YPRE = 10**Q(LIY+J-1-IFIRST+IFIRSO)
                  ELSE
                     YPRE = Q(LIY+J-1-IFIRST+IFIRSO)
                  ENDIF
               ELSE
                  YPC  = 0.
                  YPRE = 0.
               ENDIF
               IF(IOPTP.EQ.1)THEN
                  YM1 = MAX(MIN(YCUR+YADD,YPRE),YCUR)
                  YM2 = MAX(MIN(YCUR+YADD,YNXT+YNC),YCUR)
               ELSEIF(IOPTP.EQ.-1)THEN
                  YM1 = MIN(MAX(YCUR-YADD,YPRE),YCUR)
                  YM2 = MIN(MAX(YCUR-YADD,YNXT-YNC),YCUR)
               ELSE
                  IF(YCUR.GT.YADD)THEN
                     YM1 = MAX(YADD,YPC)
                     YM2 = MAX(YADD,YNC)
                  ELSE
                     YM1 = MIN(YADD,YPC)
                     YM2 = MIN(YADD,YNC)
                  ENDIF
               ENDIF
               IF(LOGYFL)THEN
                  IF(YM1.GT.0.)YM1 = LOG10(YM1)
                  IF(YM2.GT.0.)YM2 = LOG10(YM2)
               ENDIF
            ENDIF
*----
* Compute X1 X2
*----
            IF(IQ(LSDIR+5).NE.0)THEN
               IF(.NOT.LHBAR)THEN
                  X1 = Q(LIX+J-IFIRST+IFIRSO)
                  X2 = Q(LIX+J+1-IFIRST+IFIRSO)
               ELSE
                  DELTA = (Q(LIX+J-IFIRST+IFIRSO+1)
     +                    -Q(LIX+J-IFIRST+IFIRSO))
                  X1    = Q(LIX+J-IFIRST+IFIRSO)+ROFZB*DELTA
                  X2    = X1+(RWDZB*DELTA)
               ENDIF
            ELSE
               IF(.NOT.LHBAR)THEN
                  X1 = Q(LIX+1)+(J-IFIRST)*DELTAX
                  X2 = X1+DELTAX
               ELSE
                  X1 = Q(LIX+1)+((J-IFIRST)*DELTAX)+(ROFZB*DELTAX)
                  X2 = X1+(RWDZB*DELTAX)
               ENDIF
            ENDIF
            IF(LOGYFL.AND.C1.LE.0.)GOTO 20
*----
* Update the current plot
*----
            IF(Y2.NE.Y1)CALL HPLHUP(X1,Y1,X2,Y2,YM1,YM2,LHBAR)
            IF(ABS(IOPTP).EQ.1.OR.IOPTU.NE.0)Q(LIY+J-IFIRST+IFIRSO)=Y2
         ELSE
            Q(LIY+J)=YB
         ENDIF
   20 CONTINUE
*
      IF((IOPTU.NE.0).OR.(IOPTP.NE.0))GOTO 999
*----
*          Option "Update" is not selected.
*          Draw histogram according to value of IHTYP
*          if IHTYP.EQ.0 then Polyline requested
*          if IHTYP.NE.0 then Fill_area requested
*----
      IF(I6.EQ.0)THEN
         Q(LIX+1)=XMINI
         Q(LIX+2)=XMAXI
      ELSE
*----
*             Non_equidistant bins
*----
         LBINS=LQ(LCID-2)
         DO 30 I=1,ICMAX+1
            Q(LIX+I)=Q(LBINS+I)
   30    CONTINUE
      ENDIF
*----
*     Prepare Fill area (systematic with option "Barre").
*----
   40 CONTINUE
      IHTOLD=IHTYP
      IF((LOBAR).OR.(IHOPTB.NE.0))THEN
         IF(IHTYP.EQ.0.OR.IHTYP.EQ.1000)IHTYP=1001
      ENDIF
      CALL HPLATT(1)
*----
*     Code option for IGHIST
*----
      CHOPTH = ' '
      CHOPTG = ' '
      CHTMPG = ' '
      IF(IHOPTL.NE.0)CHOPTH(1:1) = 'L'
      IF(IHOPTT.NE.0)CHOPTH(2:2) = '*'
      IF(IHOPTP.NE.0)CHOPTH(3:3) = 'P'
      IF((IHOPTC.NE.0).OR.(IHOPTH.NE.0).OR.(IHOPTB.NE.0))THEN
         IF(IHOPTC.NE.0)CHOPTH(4:4) = 'C'
         IF(IHOPTH.NE.0)THEN
            CHOPTH(5:5) = 'H'
            CHOPTG      = 'H'
         ELSE IF(IHOPTB.NE.0)THEN
            CHOPTH(6:6) = 'B'
            CHOPTG      = 'B'
         ENDIF
         IF(IHTYP.NE.1000.AND.IHTYP.NE.0)THEN
            IF(LOGYFL)THEN
               CHOPTH(7:7) = '1'
               IF(CHOPTG.NE.' ')THEN
                  CHTMPG = '1'//CHOPTG
                  CHOPTG = CHTMPG
               ENDIF
            ENDIF
            IF((IHOPTH.NE.0).OR.(IHOPTC.NE.0))THEN
               CHOPTH(8:8) = 'F'
               IF(CHOPTG.NE.' ')THEN
                  CHTMPG = 'F'//CHOPTG
                  CHOPTG = CHTMPG
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF((I6.NE.0).AND.(CHOPTH.NE.' '))THEN
         CHOPTH(9:9) = 'N'
         IF(CHOPTG.NE.' ')THEN
            CHTMPG = 'N'//CHOPTG
            CHOPTG = CHTMPG
         ENDIF
      ENDIF
*
      IF(CHOPTH.EQ.' ')THEN
         IF(IHTYP.NE.1000.AND.IHTYP.NE.0)THEN
            IF(.NOT.LOGYFL)THEN
               IF(.NOT.LOBAR)THEN
                  IF(I6.EQ.0)THEN
                     CHOPTH = 'F'
                     CHOPTG = 'F   '
                  ELSE
                     CHOPTH = 'FN'
                     CHOPTG = 'FN  '
                  ENDIF
               ELSE
                  IF(I6.EQ.0)THEN
                     CHOPTH = 'B'
                     CHOPTG = 'B   '
                  ELSE
                     CHOPTH = 'BN'
                     CHOPTG = 'BN  '
                  ENDIF
               ENDIF
            ELSE
               IF(.NOT.LOBAR)THEN
                  IF(I6.EQ.0)THEN
                     CHOPTH = 'F1'
                     CHOPTG = 'F1  '
                  ELSE
                     CHOPTH = 'F1N'
                     CHOPTG = 'F1N '
                  ENDIF
               ELSE
                  IF(I6.EQ.0)THEN
                     CHOPTH = 'B1'
                     CHOPTG = 'B1  '
                  ELSE
                     CHOPTH = 'B1N'
                     CHOPTG = 'B1N '
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF(I6.EQ.0)THEN
               CHOPTH = 'H'
               CHOPTG = 'H   '
            ELSE
               CHOPTH = 'HN'
               CHOPTG = 'HN  '
            ENDIF
         ENDIF
      ENDIF
*----
*     Option LOGX
*----
      IF(LOGXFL)THEN
         CHOPTH(10:11) = 'GX'
         IF(CHOPTG.NE.' ')THEN
            CHTMPG = 'GX'//CHOPTG
            CHOPTG = CHTMPG
         ENDIF
         IF(I6.EQ.0)THEN
            Q(LIX+1) = 10**Q(LIX+1)
            Q(LIX+2) = 10**Q(LIX+2)
         ENDIF
      ENDIF
*----
*     Draw the histogram
*----
      IF(NOKEEP)THEN
         CHOPTH(16:16) = 'K'
         CALL IGHIST(NBINS,RQUEST(80),0.,CHOPTH)
         GOTO 999
      ENDIF
      LIY = LSDIR+19
      LIX = LIY+ICMAX
      IF(I6.NE.0)THEN
         IQUEST(81)    = LIX+IFIRST
         IQUEST(82)    = LIY+IFIRST
         CHOPTH(16:16) = 'Z'
         CALL IGHIST(NBINS,Q(LIX+IFIRST),Q(LIY+IFIRST),CHOPTH)
      ELSE
         IQUEST(81)    = LIX+1
         IQUEST(82)    = LIY+IFIRST
         CHOPTH(16:16) = 'Z'
         CALL IGHIST(NBINS,Q(LIX+1),Q(LIY+IFIRST),CHOPTH)
      ENDIF
*----
*     Fill bank with the specification of the histogram
*----
      IQ(LSDIR+1)=IDZB
      IQ(LSDIR+2)=NTWIN
      IF(LOGYFL)THEN
         IQ(LSDIR+3) = 1
      ELSE
         IQ(LSDIR+3) = 0
      ENDIF
      IF(LOGXFL)THEN
         IQ(LSDIR+4) = 1
      ELSE
         IQ(LSDIR+4) = 0
      ENDIF
      IQ(LSDIR+5) = I6
      IQ(LSDIR+6) = ICMAX
      CALL UCTOH(CHOPTG,IQ(LSDIR+9),4,4)
      Q(LSDIR+10) = ROFFS
      Q(LSDIR+11) = RWDTH
      Q(LSDIR+12) = XLOW
      Q(LSDIR+13) = XHIGH
      Q(LSDIR+14) = YLOW
      Q(LSDIR+15) = YHIGH
      Q(LSDIR+16) = XWMINI
      Q(LSDIR+17) = XWMAXI
      Q(LSDIR+18) = YWMINI
      Q(LSDIR+19) = YWMAXI
*----
*     If option "KEEP" is selected then keep bank
*----
      IF(IOPTK.NE.0)CALL SBIT1(IQ(LSDIR),1)
*
      IHTYP=IHTOLD
*
  999 CALL IGSET('LWID',RLWID)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('FASI',RFASI)
      CALL IGSET('FACI',RFACI)
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('BORD',RBORD)
*
      END
