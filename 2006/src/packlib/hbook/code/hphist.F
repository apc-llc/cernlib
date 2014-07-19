*
* $Id: hphist.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hphist.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      SUBROUTINE HPHIST(IDD,KNAME,NUMM)
*.==========>
*.           PRINTS A 1-DIM HIST VERTICALLY
*.           IF(IDD.EQ.0)PRINTS ALL HISTOGRAMS
*.           KNAME IS THE OPTION TO BE PRINTED(PROX,PROY,SLIX,ETC)
*.           NUM IS THE SLICE OR BAND NUMBER
*.           NUM=0  MEANS ALL SLICES OR BANDS
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      COMMON/HFORM/IFORM(128)
      CHARACTER*(*) KNAME
*.___________________________________________
      IRET=3
      NUM=NUMM
*
      CALL UCTOH(KNAME,NAME,4,4)
      ICASE=IUCOMP(NAME,IDENT,8)
*
  20  CALL HLOOP(IDD,'HPHIST',IRET)
      IF(IRET.EQ.0)GO TO 99
*
      CALL HDCOFL
      IF(I123.EQ.0)GO TO 80
      INUM=0
      IF(I9+I10+I11.NE.0)INUM=1
      NW=(1+I12+INUM)*102
*
      CALL HWORK(NW,IAD,'HPHIST')
      IF(IAD.EQ.0)GO TO 99
      IADE=IAD+102*INUM
      IADF=IADE+102*I12
      XMIN=0.
      XMAX=0.
*
*            Simple histogram
*
      IF(ICASE.LT.3.AND.I1.NE.0)THEN
         LPRX=LCID+KNCX
         LCONT=LQ(LCID-1)
         NOENT=I9
         NB=IQ(LCONT+KNBIT)
         X0=Q(LCID+KXMIN)
         XSIZE=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
*
         CALL HPR1V(Q(IAD),Q(IADE),Q(IADF),IFORM(1),IFORM(111),
     +   1,IQ(LCID+KNCX)+2,1,X0,XSIZE,XMIN,XMAX)
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
            CALL HPR1V(Q(IAD),Q(IADE),Q(IADF),IFORM(1),IFORM(111),
     +      3,IQ(LCID+KNCX)+2,1,X0,XSIZE,XMIN,XMAX)
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
            CALL HPR1V(Q(IAD),Q(IADE),Q(IADF),IFORM(1),IFORM(111),
     +      4,IQ(LCID+KNCY)+2,1,X0,XSIZE,XMIN,XMAX)
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
               CALL HPR1V(Q(IAD),Q(IADE),Q(IADF),IFORM(1),IFORM(111),
     +         5,IQ(LCID+KNCX)+2,I,X0,XSIZE,XMIN,XMAX)
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
               CALL HPR1V(Q(IAD),Q(IADE),Q(IADF),IFORM(1),IFORM(111),
     +         6,IQ(LCID+KNCY)+2,I,X0,XSIZE,XMIN,XMAX)
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
               CALL HPR1V(Q(IAD),Q(IADE),Q(IADF),IFORM(1),IFORM(111),
     +         7,IQ(LCID+KNCX)+2,NBX,X0,XSIZE,XMIN,XMAX)
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
               CALL HPR1V(Q(IAD),Q(IADE),Q(IADF),IFORM(1),IFORM(111),
     +         8,IQ(LCID+KNCY)+2,NBY,X0,XSIZE,XMIN,XMAX)
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
