*
* $Id: hindex.F,v 1.2 1998/11/10 09:18:51 couet Exp $
*
* $Log: hindex.F,v $
* Revision 1.2  1998/11/10 09:18:51  couet
* - mods for Y2K
*
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HINDEX
*.==========>
*.           prints index of histograms
*..=========> ( R.Brun )
*
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcvers.inc"
#include "hbook/hcprin.inc"
      COMMON/HFORM/IA(127),IDU
*.___________________________________________
      IF(IPONCE.EQ.2)GO TO 999
      IH=0
      NHT=0
      NH=0
*
*             PRINT GLOBAL TITLE,IF ANY
*
      CALL HFORMA(3)
      CALL VBLANK(IA,128)
      IF(LGTIT.EQ.0)GO TO 10
      J=IQ(LGTIT-1)
      IF(J.EQ.0)GO TO 10
      IF(J.GT.20)J=20
      DO 5 I=1,J
         CALL UBLOW(IQ(LGTIT+I),IA(4*I-3),4)
    5 CONTINUE
      CALL HFORMA(1)
      CALL HFORMA(2)
*
   10 CALL VFILL(IA,125,IDG(38))
      CALL HFORMA(1)
      CALL VBLANK(IA(2),123)
      CALL HFORMA(1)
*
*
*             PRINT HEADER
*
      WRITE(LOUT,9010) INSTAL,IVERS,CHDATE
*
      CALL HFORMA(1)
      CALL VFILL(IA,125,IDG(38))
      CALL HFORMA(1)
      CALL VBLANK(IA(2),123)
      CALL HFORMA(1)
*
*             PRINT EXPLANATION OF COLUMNS
*
      WRITE(LOUT,9020)
*
      CALL HFORMA(1)
      CALL VFILL(IA,125,IDG(38))
      CALL HFORMA(1)
      CALL VBLANK(IA(2),123)
      CALL HFORMA(1)
*
*             PRINT INFORMATION RELATIVE TO EACH HISTOGRAM
*
      IDD=0
      IRET=3
*
   20 CALL HLOOP(IDD,'HINDEX',IRET)
*
      IF(IRET.EQ.0)GO TO 130
      NBX=0
      NBY=0
      CALL HDCOFL
      ILINK=0
*
   25 ILINK=ILINK+1
      IF(ILINK.GT.1.AND.I1.NE.0)GO TO 120
      IF(ILINK.GT.7.AND.I1.EQ.0)GO TO 120
      LCONT=LQ(LCID-ILINK)
      IF(LCONT.EQ.0)GO TO 25
      GO TO (30,50,60,70,70,100,110),ILINK
*
*             1-DIM HISTOGRAM
*
   30 IF(I1.NE.0)THEN
         XLOW=Q(LCID+KXMIN)
         CALL HFORMA(1)
         NWTITL=(IQ(LCID-1)-KTIT1+1)
         IF(NWTITL.GT.20)NWTITL=20
         CALL VBLANK(IA(2),80)
         DO 35 I=1,NWTITL
            K=4*(I-1)+2
            CALL UBLOW(IQ(LCID+KTIT1+I-1),IA(K),4)
   35    CONTINUE
         NH=NH+1
*
         WRITE(LOUT,9800)NH,(IA(I),I=2,41),ID,IQ(LCONT+KNBIT),
     +   IQ(LCONT+KNOENT),IDG(2),IDG(34),IQ(LCID+KNCX),XLOW,
     +   Q(LCID+KXMAX),LCID,IQ(LCID+KNTOT)
*
         WRITE(LOUT,9810)(IA(I),I=42,81)
         CALL VBLANK(IA(2),123)
         GO TO 120
      ENDIF
*
*             N-tuple
*
      IF(I4.NE.0)THEN
         CALL HFORMA(1)
         NWTITL=IQ(LCID+8)
         CALL VBLANK(IA(2),80)
         ITIT1=IQ(LCID+9)
         DO 36 I=1,NWTITL
            K=4*(I-1)+2
            CALL UBLOW(IQ(LCID+ITIT1+I-1),IA(K),4)
   36    CONTINUE
         NH=NH+1
*
         WRITE(LOUT,9820)NH,(IA(I),I=2,41),ID,IDG(24),LCID,IQ(LCID-1)
*
         WRITE(LOUT,9810)(IA(I),I=42,81)
         CALL VBLANK(IA(2),123)
         GO TO 120
      ENDIF
*
*             2-DIM HISTOGRAM
*
      IF(I230.EQ.0)GO TO 120
      XLOW=Q(LCID+KXMIN)
      YLOW=Q(LCID+KYMIN)
      NH=NH+1
      NWTITL=(IQ(LCID-1)-KTIT2+1)
      NCX=IQ(LCID+KNCX)
      NCY=IQ(LCID+KNCY)
      CALL HFORMA(1)
      CALL VBLANK(IA(2),80)
      IF(NWTITL.GT.20)NWTITL=20
      DO 37 I=1,NWTITL
         K=4*(I-1)+2
         CALL UBLOW(IQ(LCID+KTIT2+I-1),IA(K),4)
   37 CONTINUE
*
      WRITE(LOUT,9800)NH,(IA(I),I=2,41),ID,IQ(LCONT+KNBIT),
     +IQ(LCONT+KNOENT),IDG(3),IDG(34),NCX,XLOW,
     +Q(LCID+KXMAX),LCID,IQ(LCID+KNTOT)
*
      NT2=IQ(LCONT-1)+10
      WRITE(LOUT,9805)(IA(I),I=42,81),IDG(35),NCY,YLOW,
     +        Q(LCID+KYMAX),LCONT,NT2
*
      CALL VBLANK(IA(2),123)
      GO TO 25
*
*             PROJECTION X
*
   50 CALL HINPRX('PROJECTION X',IDG(34),0,NCX,XLOW,Q(LCID+KXMAX),
     +LCONT)
      GO TO 25
*
*             PROJECTION Y
*
   60 CALL HINPRX('PROJECTION Y',IDG(35),0,NCY,YLOW,Q(LCID+KYMAX),
     +LCONT)
      GO TO 25
*
*             SLICES
*
   70 NSL=IQ(LCONT-2)
      DO 90 NUM=1,NSL
         LC=LQ(LCONT-NUM)
         IF(ILINK.EQ.4)THEN
            BW=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(NSL)
            YMIN=Q(LCID+KYMIN)+BW*FLOAT(NUM-1)
            YMAX=YMIN+BW
            CALL HINPRX('SLICE X     ',IDG(34),NUM,NCX,YMIN,YMAX,LC)
         ELSE
            BW=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(NSL)
            XMIN=Q(LCID+KXMIN)+BW*FLOAT(NUM-1)
            XMAX=XMIN+BW
            CALL HINPRX('SLICE Y     ',IDG(35),NUM,NCY,XMIN,XMAX,LC)
         ENDIF
   90 CONTINUE
      GO TO 25
*
*             BAND  X
*
  100 LBANX=LCONT
  105 LCONT=LQ(LBANX-1)
      NBX=NBX+1
      YMIN=Q(LBANX+1)
      YMAX=Q(LBANX+2)
      CALL HINPRX('BAND X      ',IDG(34),NBX,NCX,YMIN,YMAX,LCONT)
      LBANX=LQ(LBANX)
      IF(LBANX.NE.0)GO TO 105
      GO TO 25
*
*             BAND  Y
*
  110 LBANY=LCONT
  115 LCONT=LQ(LBANY-1)
      NBY=NBY+1
      XMIN=Q(LBANY+1)
      XMAX=Q(LBANY+2)
      CALL HINPRX('BAND Y      ',IDG(35),NBY,NCY,XMIN,XMAX,LCONT)
      LBANY=LQ(LBANY)
      IF(LBANY.NE.0)GO TO 115
      GO TO 25
*
  120 IRET=2
      GO TO 20
*
*             PRINT INFORMATION,ABOUT MEMORY AND DISK USAGE
*
  130 CALL HFORMA(1)
      CALL VFILL(IA,125,IDG(38))
      CALL HFORMA(1)
      WRITE(LOUT,9090)
      WRITE(LOUT,9092)NWPAW
      NH=0
      IF(IPONCE.NE.0)IPONCE=2
*
*
 9010 FORMAT(' .',3X,'HBOOK',3X,20A1,3X,'VERSION   ',5A1,6X,
     + 'HISTOGRAM AND PLOT INDEX  ',27X,A10,5X,'.')
 9020 FORMAT(' .',2X,'NO',21X,'TITLE',22X,'ID',2X,'B/C',2X,
     +       'ENTRIES',1X,'DIM',3X,'NCHA',5X,'LOWER',7X,'UPPER',
     +  7X,'ADDRESS',1X,'LENGTH',1X,'.')
 9800 FORMAT(' .',I4,2X,40A1,1X,I7,2X,I2,I9,2(2X,A1),
     +  I6,2(2X,E10.3),3X,I9,I7,1X,'.')
 9805 FORMAT(' .',6X,40A1,26X,A1,I6,2X,E10.3,2X,E10.3,
     + 3X,I9,I7,1X,'.')
 9810 FORMAT(' .',6X,40A1,77X,'.')
 9820 FORMAT(' .',I4,2X,40A1,I8,15X,A1,36X,I9,I7,1X,'.')
 9090 FORMAT(///,' MEMORY UTILISATION',/)
 9092 FORMAT(' ',5X,'MAXIMUM TOTAL SIZE OF COMMON /PAWC/       ',I10)
  999 RETURN
      END
