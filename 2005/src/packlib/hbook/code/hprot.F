*
* $Id: hprot.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprot.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      SUBROUTINE HPROT(IDD,KNAME,NUMM)
*.==========>
*.           PRINTS A 1-DIM HIST ROTATES
*.           IF(IDD.EQ.0)PRINTS ALL HISTOGRAMS
*.           KNAME IS THE OPTION TO BE PRINTED(PROX,PROY,SLIX,ETC)
*.           NUM IS THE SLICE OR BAND NUMBER
*.           NUM=0  MEANS ALL SLICES OR BANDS
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      CHARACTER*(*) KNAME
*.___________________________________________
      IRET=3
      NUM=NUMM
*
      CALL UCTOH(KNAME,NAME,4,4)
      ICASE=IUCOMP(NAME,IDENT,8)
*
  20  CALL HLOOP(IDD,'HPROT ',IRET)
      IF(IRET.EQ.0)GO TO 99
*
      CALL HDCOFL
      IF(I123.EQ.0)GO TO 80
*
      CALL HWORK(165,IAD,'HPROT ')
      IF(IAD.EQ.0)GO TO 99
      XMIN=0.
      XMAX=0.
*
*            Simple histogram
*
      IF(ICASE.LT.3.AND.I1.NE.0)THEN
         LCONT=LQ(LCID-1)
         LPRX=LCID+KNCX
         NOENT=I9
         NB=IQ(LCONT+KNBIT)
         X0=Q(LCID+KXMIN)
         XSIZE=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
         CALL HPR1H(1,IQ(LCID+KNCX),1,X0,XSIZE,XMIN,XMAX,
     +            Q(IAD),Q(IAD+105))
      ENDIF
      IF(I230.EQ.0)GO TO 80
*
*             Projection X
*
      IF(ICASE.EQ.3.OR.ICASE.EQ.0)THEN
         LCONT=LQ(LCID-2)
         LPRX=LCID+KNCX
         IF(LCONT.NE.0)THEN
            NOENT=I9
            NB=IQ(LCONT+KNBIT)
            X0=Q(LCID+KXMIN)
            XSIZE=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
            CALL HPR1H(3,IQ(LCID+KNCX),1,X0,XSIZE,XMIN,XMAX,
     +               Q(IAD),Q(IAD+105))
         ENDIF
      ENDIF
*
*             Projection Y
*
      IF(ICASE.EQ.4.OR.ICASE.EQ.0)THEN
         LCONT=LQ(LCID-3)
         LPRX=LCID+KNCY
         IF(LCONT.NE.0)THEN
            NOENT=I10
            NB=IQ(LCONT+KNBIT)
            X0=Q(LCID+KYMIN)
            XSIZE=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
            CALL HPR1H(4,IQ(LCID+KNCY),1,X0,XSIZE,XMIN,XMAX,
     +               Q(IAD),Q(IAD+105))
         ENDIF
      ENDIF
*
*             Slices X
*
      IF(ICASE.EQ.5.OR.ICASE.EQ.0)THEN
         LSLIX=LQ(LCID-4)
         LPRX=LCID+KNCX
         IF(LSLIX.NE.0)THEN
            BW=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LSLIX-2))
            DO 25 I=1,IQ(LSLIX-2)
               IF(NUM.NE.0.AND.NUM.NE.I)GO TO 25
               LCONT=LQ(LSLIX-I)
               NOENT=I9
               NB=IQ(LCONT+KNBIT)
               X0=Q(LCID+KXMIN)
               XSIZE=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
               XMIN=Q(LCID+KYMIN)+BW*FLOAT(I-1)
               XMAX=XMIN+BW
               CALL HPR1H(5,IQ(LCID+KNCX),I,X0,XSIZE,XMIN,XMAX,
     +                  Q(IAD),Q(IAD+105))
  25        CONTINUE
         ENDIF
      ENDIF
*
*             Slices Y
*
      IF(ICASE.EQ.6.OR.ICASE.EQ.0)THEN
         LSLIY=LQ(LCID-5)
         LPRX=LCID+KNCY
         IF(LSLIY.NE.0)THEN
            BW=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LSLIY-2))
            DO 30 I=1,IQ(LSLIY-2)
               IF(NUM.NE.0.AND.NUM.NE.I)GO TO 30
               LCONT=LQ(LSLIY-I)
               NOENT=I10
               NB=IQ(LCONT+KNBIT)
               X0=Q(LCID+KYMIN)
               XSIZE=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
               XMIN=Q(LCID+KXMIN)+BW*FLOAT(I-1)
               XMAX=XMIN+BW
               CALL HPR1H(6,IQ(LCID+KNCY),I,X0,XSIZE,XMIN,XMAX,
     +                  Q(IAD),Q(IAD+105))
  30        CONTINUE
         ENDIF
      ENDIF
*
*              Bande X
*
      IF(ICASE.EQ.7.OR.ICASE.EQ.0)THEN
         LBANX=LQ(LCID-6)
         LPRX=LCID+KNCX
         IF(LBANX.NE.0)THEN
            NBX=1
  40        LCONT=LQ(LBANX-1)
            IF(NUM.EQ.0.OR.NUM.EQ.NBX)THEN
               NOENT=I9
               NB=IQ(LCONT+KNBIT)
               X0=Q(LCID+KXMIN)
               XSIZE=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
               XMIN=Q(LBANX+1)
               XMAX=Q(LBANX+2)
               CALL HPR1H(7,IQ(LCID+KNCX),NBX,X0,XSIZE,XMIN,XMAX,
     +                  Q(IAD),Q(IAD+105))
            ENDIF
            NBX=NBX+1
            LBANX=LQ(LBANX)
            IF(LBANX.NE.0)GO TO 40
         ENDIF
      ENDIF
*
*             Bande Y
*
      IF(ICASE.EQ.8.OR.ICASE.EQ.0)THEN
         LBANY=LQ(LCID-7)
         LPRX=LCID+KNCY
         IF(LBANY.NE.0)THEN
            NBY=1
  50        LCONT=LQ(LBANY-1)
            IF(NUM.EQ.0.OR.NUM.EQ.NBY)THEN
               NOENT=I10
               NB=IQ(LCONT+KNBIT)
               X0=Q(LCID+KYMIN)
               XSIZE=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
               XMIN=Q(LBANY+1)
               XMAX=Q(LBANY+2)
               CALL HPR1H(8,IQ(LCID+KNCY),NBY,X0,XSIZE,XMIN,XMAX,
     +                  Q(IAD),Q(IAD+105))
            ENDIF
            NBY=NBY+1
            LBANY=LQ(LBANY)
            IF(LBANY.NE.0)GO TO 50
         ENDIF
      ENDIF
*
  80  IRET=2
      IF(IPONCE.NE.0.AND.I230.EQ.0)CALL SBYT(0,IQ(LCID+KBITS),1,3)
      GO TO 20
*
  99  RETURN
      END
