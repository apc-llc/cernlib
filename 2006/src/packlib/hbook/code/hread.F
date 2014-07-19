*
* $Id: hread.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hread.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HREAD(IDD,LUN,MABUFF,LENGTH,MODEE,NOMORE)
*.==========>
*.       read into memory the contents of histogram IDD
*.       from LUN in machine independent format.
*.
*.       Routine provided in HBOOK4 to read histograms from version 3.
*.       All parameters but IDD and LUN are not used anymore.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcbits.inc"
      COMMON/HCGARB/JTEMP(10),KEEP1,KEEP2,LOW1,LOW2,LO1,LO2,LTITLE
      CHARACTER*80 CHTIT
      DIMENSION TEMP(8)
      EQUIVALENCE (XMIN,TEMP(4)) , (XMAX,TEMP(5))
      EQUIVALENCE (YMIN,TEMP(7)) , (YMAX,TEMP(8))
C
      COMMON/HMIBUF/MISCUR,NBUFMI,LUNTYP,NEWBUF,LCBUFF,LUBUFF,MIBEND
C
      DIMENSION ISPEC(11)
      SAVE ISPEC
C     DATA ISPEC/1H!,1H?,1H^,1H>,1H<,1H[,1H],1H&,1H",1H#,1H$/
#if defined(CERNLIB_CDC)
      DATA ISPEC/
     +           O"66555555555555555555"
     +          ,O"71555555555555555555"
     +          ,O"76555555555555555555"
     +          ,O"73555555555555555555"
     +          ,O"72555555555555555555"
     +          ,O"61555555555555555555"
     +          ,O"62555555555555555555"
     +          ,O"67555555555555555555"
     +          ,O"64555555555555555555"
     +          ,O"60555555555555555555"
     +          ,O"63555555555555555555"/
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_IBMMVS)
      DATA ISPEC/
     +     Z5A404040,Z6F404040,Z6A404040,Z6E404040,Z4C404040,
     +     ZAD404040,ZBD404040,Z50404040,Z7F404040,Z7B404040,
     +     Z7A404040/
#endif
#if defined(CERNLIB_NORD)
      DATA ISPEC/
     +     4110020040B, 7710020040B,13610020040B, 7610020040B,
     +     7410020040B,13310020040B,13510020040B, 4510020040B,
     +     4210020040B, 4310020040B, 7210020040B/
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_OLD))
      DATA ISPEC/
     +     Z20202021,Z2020203F,Z2020205E,Z2020203E,Z2020203C,
     +     Z2020205B,Z2020205D,Z20202025,Z20202022,Z20202023,
     +     Z2020203A/
#endif
#if defined(CERNLIB_VAX)
      DATA ISPEC/
     +     '20202021'X,'2020203F'X,'2020205E'X,'2020203E'X,'2020203C'X,
     +     '2020205B'X,'2020205D'X,'20202025'X,'20202022'X,'20202023'X,
     +     '2020203A'X/
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)||defined(CERNLIB_UNIVAC)
      DATA ISPEC/1H!,1H?,1H^,1H>,1H<,1H[,1H],1H&,1H",1H#,1H$/
#endif
*.___________________________________________
C
C             Reserve 1000 words of working space
C
      LUBUFF = 945
      LCBUFF = LUBUFF*32 / NBIT
      CALL HWORK(1000,MEMSWA,'HREAD ')
      IF(MEMSWA.EQ.0) GO TO 999
*
  100 NEWBUF = 1
      ID = 0
      NBUFMI = 4
      LUNTYP = -1
C
C             Read in the first record
C
      CALL HUPKRD(LUN,NEXT,MEMSWA)
      IF(NBUFMI.EQ.0) GO TO 997
      IF(IQ(MISCUR).EQ.0) GO TO 996
*
      IF(IQ(MISCUR).EQ.0) GO TO 997
      IF(IDD.EQ.0) GO TO 200
      ID = IDD
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,IDD)
      IF(IDPOS.GT.0)THEN
C
C             ID NO. already exists
C
         NEWID = ID
         CALL HBUG('Already existing histogram replaced','HREAD ',NEWID)
         GO TO 999
      ENDIF
C
C             Search for the required histogram on tape
C
  130 IF(IQ(MEMSWA+3).EQ.0) GO TO 140
C
C             A new id starts somewhere in this record
C
      MISCUR = MEMSWA+IQ(MEMSWA+3)
  160 IF(IQ(MISCUR).EQ.0) GO TO 996
      NEXT = MISCUR+1
      IF(IQ(NEXT).NE.0) GO TO 150
C
C             New id header section encountered
C
      ID = HPKR32(IQ(NEXT+1))
      IF(ID.EQ.IDD) GO TO 210
C
C             Not the required histogram, move onto the next section
C
  150 MISCUR = MISCUR+IQ(MISCUR)
      IF(MISCUR.LT.MIBEND) GO TO 160
C
C             Read in the next record
C
  140 CALL HUPKRD(LUN,NEXT,MEMSWA)
      IF(NBUFMI.EQ.0) GO TO 997
      GO TO 130
  200 IF(IQ(MISCUR).EQ.0) GO TO 997
      IF(IQ(NEXT).NE.0) GO TO 997
      ID = HPKR32(IQ(NEXT+1))
  210 NEXT = NEXT+2
      IF(ID.NE.0) GO TO 300
C
C             Global title - reserve space and create it
C
      LTITLE = IQ(NEXT)-2
      CHTIT=' '
      NCH=MIN(79,LTITLE)
      CHTIT(NCH+1:NCH+1)='$'
C
C             Title requires I words for LTITLE characters
C
      CALL HTITLE(CHTIT)
      NEXT = NEXT+1
      LOW1 = LGTIT+1
      NCHT = 0
      LOW2 = 0
      LO1 = 0
  215 CALL VBLANK(JTEMP,NCHAR)
  220 NEXT=NEXT+1
      IF(NEXT.LE.MIBEND)GO TO 240
      CALL HUPKRD(LUN,NEXT,MEMSWA)
      IF(NBUFMI.EQ.0)GO TO 997
      IF(IQ(MISCUR).EQ.0)GO TO 997
      NEXT=NEXT-1
C
  240 LOW2=LOW2+1
      IBN=IQ(NEXT)
      IF(IBN.LT.50)JTEMP(LOW2)=IDG(IBN)
      IF(IBN.GT.50)JTEMP(LOW2)=ISPEC(IBN-50)
      NCHT=NCHT+1
      LTITLE=LTITLE-1
      IF(LTITLE.EQ.0)GO TO 260
      IF(LOW2.NE.NCHAR)GO TO 220
C
  260 CONTINUE
      IF(NCHT.LE.80)CALL UBUNCH(JTEMP,IQ(LOW1),NCHAR)
      LOW1=LOW1+1
      LOW2=0
      CALL VBLANK(JTEMP,NCHAR)
      IF(LTITLE.NE.0)GO TO 220
C
  280 MISCUR=NEXT+1
      NEXT=MISCUR+1
      IF(NEXT.LE.MIBEND)GO TO 285
      CALL HUPKRD(LUN,NEXT,MEMSWA)
      IF(NBUFMI.EQ.0)GO TO 997
      IF(IQ(MISCUR).EQ.0)GO TO 997
  285 IF(LO1.NE.0)GO TO 440
      GO TO 200
C
  300 NEWID = ID
      IF(IDD.NE.0) GO TO 310
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,IDD)
      IF(IDPOS.GT.0)THEN
C
C             ID NO. already exists
C
         NEWID = ID
         CALL HBUG('Already existing histogram replaced','HREAD ',NEWID)
         GO TO 999
      ENDIF
  310 NEXT = NEXT+1
      IF(IQ(NEXT).NE.13) GO TO 997
C
C             Store the dimensions of the histogram
C
      NEXT = NEXT+1
      CALL UCOPY(IQ(NEXT),TEMP,8)
      DO 330 I = 3,8
         TEMP(I) = HPKR32(TEMP(I))
  330 CONTINUE
      NCHX = TEMP(3)
      NCHY = TEMP(6)
      NEXT = NEXT+8
      VMX = 0.0
      I = IQ(NEXT)
      IF(I.LT.32) VMX = 2.0**I-1.0
      I1 = JBIT(TEMP(1),1)
      I2 = JBIT(TEMP(1),2)
      I3 = JBIT(TEMP(1),3)
      I230 = I2+I3
      I123 = I1+I230
      MISCUR = NEXT+1
      NEXT = MISCUR+1
      IF(MISCUR.LT.MIBEND) GO TO 340
      CALL HUPKRD(LUN,NEXT,MEMSWA)
      IF(NBUFMI.EQ.0) GO TO 997
      IF(IQ(MISCUR).EQ.0) GO TO 997
  340 IF(I123.EQ.0) GO TO 400
C
C             Check if there's a title for the histogram
C
      I = 0
      IF(IQ(NEXT).NE.12) GO TO 350
      K = IQ(MISCUR)-2
      I = K/NCHAR
      J = MOD(K,NCHAR)
      IF(J.NE.0) I = I+1
      NCH=MIN(79,K)
      CHTIT=' '
      CHTIT(NCH+1:NCH+1)='$'
C
C             BOOK IT
C
  350 IF(I230.NE.0) GO TO 360
      CALL HBOOK1(NEWID,CHTIT,NCHX,XMIN,XMAX,VMX)
      IF(IERR.EQ.0) GO TO 380
      GO TO 999
  360 IF(I2.EQ.0) GO TO 370
      CALL HBOOK2(NEWID,CHTIT,NCHX,XMIN,XMAX,NCHY,YMIN,YMAX,VMX)
      IF(IERR.EQ.0) GO TO 380
      GO TO 999
  370 CALL HTABLE(NEWID,CHTIT,NCHX,XMIN,XMAX,NCHY,YMIN,YMAX,VMX)
      IF(IERR.NE.0) GO TO 999
C
C             Store part of the status word
C
  380 CONTINUE
      KEEP1 = JBYT(TEMP,4,3)
      KEEP2 = JBYT(TEMP,11,1)
      LOW1  = JBYT(TEMP,13,4)
      LOW2  = JBYT(TEMP,22,11)
      CALL SBYT(KEEP1,IQ(LCID+KBITS),4,3)
      CALL SBYT(KEEP2,IQ(LCID+KBITS),11,1)
      CALL SBYT(LOW1,IQ(LCID+KBITS),13,4)
      CALL SBYT(LOW2,IQ(LCID+KBITS),22,11)
      IF(I.EQ.0) GO TO 440
C
C              Individual title - create it and store it
C
      LTITLE = K
      IF(I230.EQ.0)THEN
         LOW1 = LCID+KTIT1
      ELSE
         LOW1 = LCID+KTIT2
      ENDIF
      NCHT =0
      LOW2 = 0
      LO1 = 1
      GO TO 215
C
C             CALL HARRAY(ID,HOWLONG,FROMWHERE)
C
  400 CALL HARRAY(NEWID,NCHX,I)
      IF(IERR.NE.0) GO TO 999
      KEEP1 = I+1
      J = I+NCHX
  410 LOW1 = KEEP1
      DO 430 I = LOW1,J
         NEXT = NEXT+1
         Q(I) = HPKR32(IQ(NEXT))
         IF(NEXT.NE.MIBEND) GO TO 430
         NEWBUF = 0
         JUMP = 6
         KEEP1 = I+1
         IF(I.EQ.J)THEN
            NEWBUF = 1
            JUMP = 7
         ENDIF
         CALL HUPKRD(LUN,NEXT,MEMSWA)
         IF(NBUFMI.EQ.0) GO TO 997
         IF(IQ(MISCUR).EQ.0) GO TO 997
         IF(JUMP.NE.6) GO TO 990
  430 CONTINUE
      MISCUR = NEXT+1
      NEXT = MISCUR+1
      GO TO 990
C
C             Now build the histograms
C
  440 IF(JBIT(TEMP(1),7).EQ.0) GO TO 450
C
C             HBSTAT FLAG SET
C
      CALL HIDOPT(NEWID,'STAT')
      IF(IERR.NE.0) GO TO 999
  450 IF(JBIT(TEMP(1),9).EQ.0) GO TO 460
C
C             BAR(X) FLAG SET
C
      CALL HBARX(NEWID)
      IF(IERR.NE.0) GO TO 999
  460 IF(JBIT(TEMP(1),10).EQ.0) GO TO 480
C
C             BAR(Y) FLAG SET
C
      CALL HBARY(NEWID)
      IF(IERR.NE.0) GO TO 999
  480 KARYON = 0
      NUMOPT = 1
      GO TO 500
  490 CALL HUPKRD(LUN,NEXT,MEMSWA)
      IF(NBUFMI.EQ.0) GO TO 997
  500 IF(I230.NE.0) GO TO 700
C
C             1-D HISTOGRAMS
C
      IF(KARYON.NE.0) GO TO (525,540,560,580,600,610,990) , KARYON
      KARYON = 1
      IF(JBIT(TEMP(1),7).EQ.0) GO TO 525
C
C             HBSTAT FLAG SET
C
  510 CONTINUE
      DO 520 K = 1,5
         NEXT = NEXT+1
**         Q(K) = HPKR32(IQ(NEXT))
  520 CONTINUE
      MISCUR = NEXT+1
      IF(MISCUR.GT.MIBEND) GO TO 490
      NEXT = MISCUR+1
      GO TO 500
C
C             Fill the channels
C
  525 LOPCON = NCHX+3
  530 CONTINUE
  535 CONTINUE
      NB=IQ(LCONT+KNBIT)
      KTYPE = IQ(NEXT)
      NEXT = NEXT+1
C
C             First, store the no. of entries in the option
C
      ENTOPT = HPKR32(IQ(NEXT))
      KEEP1 = 1
      KARYON = 2
      I7 = 0
C
C             NOW THE CONTENTS
C
  540 LOW1 = KEEP1
      LPRX = LCID+KNCX
      IF(KTYPE.EQ.4.OR.KTYPE.EQ.6.OR.KTYPE.EQ.8)LPRX=LCID+KNCY
      DO 550 I = LOW1,LOPCON
         NEXT = NEXT+1
         IF(KTYPE.EQ.10) VALUE = HPKR32(IQ(NEXT))
         IF(KTYPE.EQ.11) VALUE = FLOAT(IQ(NEXT))
         IF(I.NE.LOPCON)  CALL HFCX(I-1,VALUE)
         IF(NEXT.NE.MIBEND) GO TO 550
         KEEP1 = I+1
         NEWBUF = 0
         IF(I.NE.LOPCON) GO TO 490
         KARYON = 3
         NEWBUF = 1
         GO TO 490
  550 CONTINUE
      MISCUR = NEXT+1
      NEXT = MISCUR+1
      KARYON = 3
  560 IQ(LCONT+KNOENT) = ENTOPT
      I7 = JBIT(TEMP(1),7)
      IF(I230.NE.0) GO TO 700
      IF(JBIT(TEMP(1),9).EQ.0) GO TO 990
C
C             ENTRIES FOR BAR(X) FOLLOWING
C
      KARYON = 4
      NW = (NCHX+2) / (NBIT/NB) + 1
      KEEP1 = 1
  580 LOW1 = KEEP1
      LW=LQ(LCONT)
      DO 590 I = LOW1,LOPCON
         NEXT = NEXT+1
         VALUE = HPKR32(IQ(NEXT))
         VALUE = VALUE*VALUE
         IF(I.GT.LOW1.AND.I.LT.LOPCON-1)Q(LW+I-1)=VALUE
         IF(NEXT.NE.MIBEND) GO TO 590
         KEEP1 = I+1
         NEWBUF = 0
         IF(I.NE.LOPCON) GO TO 490
         KARYON = 5
         NEWBUF = 1
         GO TO 490
  590 CONTINUE
      MISCUR = NEXT+1
      NEXT = MISCUR+1
      KARYON = 5
  600 IF(I230.NE.0) GO TO 700
      IF(JBIT(TEMP(1),12).EQ.0) GO TO 990
C
C             Superimposition of function flag set.
C             (NV = 1 stops HFUNC sampling a non-existent function)
C
      LO1 = NV
      NV = 1
      CALL HFUNC(NEWID,SQRT)
      IF(IERR.NE.0) GO TO 999
      NV = LO1
      KEEP1 = 2
      KARYON = 6
      J = NCHX+1
  610 LOW1 = KEEP1
      LFUNC=LQ(LCONT-1)
      DO 620 I = LOW1,J
         NEXT = NEXT+1
         VALUE = HPKR32(IQ(NEXT))
         IF(I.GT.LOW1.AND.I.LT.J-1)Q(LFUNC+I+1)=VALUE
         IF(NEXT.NE.MIBEND) GO TO 620
         KEEP1 = I+1
         NEWBUF = 0
         IF(I.NE.J) GO TO 490
         KARYON = 7
         NEWBUF = 1
         GO TO 490
  620 CONTINUE
      MISCUR = NEXT+1
      NEXT = MISCUR+1
      GO TO 990
C
C             2-D HISTOGRAM (SCATTER PLOT OR TABLE)
C
  700 CONTINUE
      GO TO (705,705,760,760,770,770,780,780) , NUMOPT
  705 IF(KARYON.EQ.1) GO TO 710
      IF(KARYON.EQ.2) GO TO 740
      KARYON = 1
C
C             Fill the channels
C
      LCONT=LQ(LCID-1)
      LSCAT=LCONT
      NB = IQ(LCONT+KNBIT)
      KTYPE = IQ(NEXT)
      NEXT = NEXT+1
C
C             How many entries are there...
C
      ENTOPT = HPKR32(IQ(NEXT))
      M = NCHX+3
      N = NCHY+3
C
C             THERE ARE (NCHX+3)*(NCHY+3) CELLS
C
      KEEP1 = 1
      KEEP2 = 1
  710 LOW1 = KEEP1
      LOW2 = KEEP2
      DO 730 I = LOW1,M
         DO 720 J = LOW2,N
            NEXT = NEXT+1
            IF(KTYPE.EQ.10) VALUE = HPKR32(IQ(NEXT))
            IF(KTYPE.EQ.11) VALUE = FLOAT(IQ(NEXT))
            IF(I.NE.M.AND.J.NE.N)CALL HFCXY(I-1,J-1,VALUE)
            IF(NEXT.NE.MIBEND) GO TO 720
            NEWBUF = 0
            KEEP1 = I
            KEEP2 = J+1
            IF(J.NE.N) GO TO 490
            KEEP1 = KEEP1+1
            KEEP2 = 1
            IF(I.NE.M) GO TO 490
            KARYON = 2
            NEWBUF = 1
            GO TO 490
  720    CONTINUE
         LOW2 = 1
  730 CONTINUE
      MISCUR = NEXT+1
      NEXT = MISCUR+1
  740 IQ(LCONT+KNOENT) = ENTOPT
      IF(JBIT(TEMP(1),8).EQ.0) GO TO 990
C
C             The ID has one or more options as well
C
  750 KARYON = 0
      IF(IQ(MISCUR).EQ.0) GO TO 999
      NUMOPT = IQ(NEXT)
      IF(NUMOPT.EQ.0) GO TO 990
      IF(NUMOPT.EQ.10) GO TO 990
      NEXT = NEXT+1
      VMX = 0.0
      I = IQ(NEXT)
      IF(I.LT.32) VMX = 2.0**I-1.0
      IF(NUMOPT.GT.4) GO TO 752
      IF(NUMOPT.EQ.3)THEN
         CALL HBPROX(NEWID,VMX)
         LCONT=LPROX
      ENDIF
      IF(NUMOPT.EQ.4)THEN
         CALL HBPROY(NEWID,VMX)
         LCONT=LPROY
      ENDIF
      IF(IERR.NE.0) GO TO 999
      GO TO 754
  752 IF(NUMOPT.GT.6) GO TO 753
      NEXT = NEXT+1
      NSLICE = IQ(NEXT)
      IF(NUMOPT.EQ.5)THEN
         CALL HBSLIX(NEWID,NSLICE,VMX)
      ENDIF
      IF(NUMOPT.EQ.6)THEN
         CALL HBSLIY(NEWID,NSLICE,VMX)
      ENDIF
      IF(IERR.NE.0) GO TO 999
      GO TO 754
  753 NEXT = NEXT+1
      XMIN = HPKR32(IQ(NEXT))
      NEXT = NEXT+1
      XMAX = HPKR32(IQ(NEXT))
      IF(NUMOPT.EQ.7)THEN
         CALL HBANDX(NEWID,XMIN,XMAX,VMX)
         LCONT=LQ(LBANX-1)
      ENDIF
      IF(NUMOPT.EQ.8)THEN
         CALL HBANDY(NEWID,XMIN,XMAX,VMX)
         LCONT=LQ(LBANY-1)
      ENDIF
      IF(IERR.NE.0) GO TO 999
  754 CONTINUE
C
C             NOWOPT points to the start of the newly created option
C
  756 MISCUR = NEXT+1
      IF(MISCUR.GT.MIBEND) GO TO 490
      NEXT = MISCUR+1
      GO TO 700
C
C             PROJECTIONS ON X OR Y
C
  760 CONTINUE
      IF(KARYON.NE.0) GO TO (761,540,763,580,750) , KARYON
      KARYON = 1
C
C             IS THE HBSTAT FLAG SET...
C
      IF(JBIT(TEMP(1),7).EQ.1) GO TO 510
  761 LOPCON = NCHX
      IF(NUMOPT.EQ.4) LOPCON = NCHY
  762 LOPCON = LOPCON+3
      GO TO 530
  763 IF(NUMOPT.EQ.3.AND.JBIT(TEMP(1),9).EQ.0) GO TO 750
      IF(NUMOPT.EQ.4.AND.JBIT(TEMP(1),10).EQ.0) GO TO 750
C
C             BAR(X) OR BAR(Y) FLAG SET
C
  764 NW = (LOPCON-1) / (NBIT/NB) + 1
      KEEP1 = 1
      KARYON = 4
      GO TO 580
C
C             SLICES ALONG X OR Y
C
  770 CONTINUE
      IF(KARYON.NE.0) GO TO (773,540,774,580,771) , KARYON
      NCSLYC = 0
  771 IF(NCSLYC.EQ.NSLICE) GO TO 750
      KARYON = 1
      NCSLYC = NCSLYC+1
      IF(NUMOPT.EQ.5)LCONT=LQ(LSLIX-NCSLYC)
      IF(NUMOPT.EQ.6)LCONT=LQ(LSLIY-NCSLYC)
      IF(JBIT(TEMP(1),7).EQ.0) GO TO 773
C
C             HBSTAT FLAG SET
C
      DO 772 K = 1,6
         NEXT = NEXT+1
**         Q(K) = HPKR32(IQ(NEXT))
  772 CONTINUE
      MISCUR = NEXT+1
      IF(MISCUR.GT.MIBEND) GO TO 490
      NEXT = MISCUR+1
C
C             Restore the contents of the slice
C
  773 CONTINUE
      LOPCON = NCHX
      IF(NUMOPT.EQ.6) LOPCON = NCHY
      LOPCON = LOPCON+3
      GO TO 535
  774 IF(NUMOPT.EQ.5.AND.JBIT(TEMP(1),9).EQ.0) GO TO 771
      IF(NUMOPT.EQ.6.AND.JBIT(TEMP(1),10).EQ.0) GO TO 771
C
C             BAR(X) OR BAR(Y) FLAG SET
C
      GO TO 764
C
C             BANDS ON X OR Y
C
  780 CONTINUE
      IF(KARYON.NE.0) GO TO (782,540,783,580,750) , KARYON
      KARYON = 1
C
C             IS THE HBSTAT FLAG SET...
C
      IF(JBIT(TEMP(1),7).EQ.0) GO TO 782
      DO 781 K = 1,5
         NEXT = NEXT+1
**         Q(K) = HPKR32(IQ(NEXT))
  781 CONTINUE
      MISCUR = NEXT+1
      IF(MISCUR.GT.MIBEND) GO TO 490
      NEXT = MISCUR+1
  782 LOPCON = NCHX
      IF(NUMOPT.EQ.8) LOPCON = NCHY
      GO TO 762
  783 IF(NUMOPT.EQ.7.AND.JBIT(TEMP(1),9).EQ.0) GO TO 750
      IF(NUMOPT.EQ.8.AND.JBIT(TEMP(1),10).EQ.0) GO TO 750
C
C             BAR(X) OR BAR(Y) FLAG SET
C
      GO TO 764
  990 I = JBYT(TEMP(1),17,5)
      IF(I.EQ.0) GO TO 995
C
C             There are a few editing constants as well
C
      IF(JBIT(TEMP(1),17).EQ.0) GO TO 991
      NEXT = NEXT+1
      J = HPKR32(IQ(NEXT))+0.1
      CALL HBIGBI(NEWID,J)
      IF(IERR.NE.0) GO TO 999
  991 IF(JBIT(TEMP(1),18).EQ.0) GO TO 992
      NEXT = NEXT+1
      VALUE = HPKR32(IQ(NEXT))
      CALL HNORMA(NEWID,HPKR32(IQ(NEXT)))
      IF(IERR.NE.0) GO TO 999
  992 IF(JBIT(TEMP(1),19).EQ.0) GO TO 993
      NEXT = NEXT+1
      CALL HSCALE(NEWID,HPKR32(IQ(NEXT)))
      IF(IERR.NE.0) GO TO 999
  993 IF(JBIT(TEMP(1),20).EQ.0) GO TO 994
      NEXT = NEXT+1
      CALL HMAXIM(NEWID,HPKR32(IQ(NEXT)))
      IF(IERR.NE.0) GO TO 999
  994 IF(JBIT(TEMP(1),21).EQ.0) GO TO 995
      NEXT = NEXT+1
      CALL HMINIM(NEWID,HPKR32(IQ(NEXT)))
      IF(IERR.NE.0) GO TO 999
C
C             end of histogram, any more to do...
C
  995 IF(IDD.NE.0) GO TO 999
      IF(I.EQ.0) GO TO 200
      MISCUR = NEXT+1
      IF(MISCUR.GT.MIBEND) GO TO 100
      NEXT = MISCUR+1
      GO TO 200
C
C             required id not on the tape
C
  996 CALL HBUG('Unknown histogram','HREAD',IDD)
      GO TO 999
C
C             tape format error - abnormal termination
C
  997 IF(IDD.EQ.0) GO TO 999
      NEWID = ID
      CALL HBUG('Error while reading','HREAD',NEWID)
  999 RETURN
      END
