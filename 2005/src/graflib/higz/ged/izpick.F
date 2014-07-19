*
* $Id: izpick.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: izpick.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/05 27/02/89  15.05.46  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZPICK(NT,SNAME,NPRIM,CHOPT)
*.===========>
*.
*. This subroutine returns the pathname of a structure .
*.
*. _Input parameters:
*.
*.  INTEGER NT : Transformation number .
*.  CHARACTER CHOPT : Type of object to be picked .
*.
*. _Output parameters:
*.
*.  CHARACTER SNAME : Structure name .
*.  INTEGER NPRIM : Adress of the primitive in the NT bank.
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hicode.inc"
#include "higz/hiatt.inc"
#include "higz/hiaca.inc"
#include "higz/hiflag.inc"
      CHARACTER*(*) SNAME,CHOPT
      DIMENSION IOPT(1)
      DIMENSION X(10),Y(10)
      EQUIVALENCE (IOPTN,IOPT(1))
*.______________________________________
*
      CALL UOPTC (CHOPT,'N',IOPT)
      NPRIM=0
      SNAME=' '
      IF(LPICD.LE.0)GOTO 100
      LPSAV=LPICD
      CALL IZSCPI(LPICD)
*
      IF(IOPTN.NE.0)THEN
         PX=RQUEST(13)
         PY=RQUEST(14)
         NT=IQUEST(10)
         IBN=1
      ELSE
         CALL IGLOC(1,NT,IBN,XNDC,YNDC,PX,PY)
      ENDIF
      IF(IBN.EQ.0)GOTO 100
*
      LN=IZGNTP(NT)
      IF(LN.EQ.0)GOTO 100
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISELNT(NT)
      IREPTR=9
      IIAA=IZGADR(LHNT,8)
      IRAA=IZGADR(LHNT,9)
*
      TEXTHE=Q(LHF+IRAA+ICHHCO-50)
      IALG=IQ(LHI+IIAA+ITXACO-80)
      IALH=INT(IALG/10.)
      DX=0.
      DY=0.
*
   10 IREPTR=IREPTR+1
      IADRI5=IREPTR
      ICHOPT=-1
      KATT=0
      IF(IREPTR.EQ.IQ(LHNT+4))GOTO 100
      ICOD=IZGCOD(LHNT,IREPTR)
      IADRI=IZGADR(LHNT,IREPTR)
*
      IF(ICOD.LT.0)THEN
         NBNOP=1
         IADRI2=ABS(IZGADR(LHNT,IREPTR))
   20    IF(IQ(LHNT+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LHNT,IADRI2))
            GOTO 20
         ENDIF
         IADRI2=IZGADR(LHNT,IADRI2)
         DO 40 I=1,NBNOP
            IF(IQ(LHI+IADRI2).EQ.0)GOTO 10
            IF(IZGCOD(LHI,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LHI,IADRI2)
               DX=DX+Q(LHF+IADRI4)
               DY=DY+Q(LHF+IADRI4+1)
            ENDIF
            IF(IQ(LHI+IADRI2).EQ.1.AND.KATT.EQ.0)THEN
               IADRI5=ABS(IZGADR(LHNT,IQ(LHI+IADRI2+2)))
               CALL IZSCAN(LHNT,IANGCO,IADRI5,-1,IPOS)
               IF(IPOS.EQ.0)GOTO 30
               IRR=IZGADR(LHNT,IPOS)
               CALL IZSCAN(LHNT,ITXACO,IADRI5,-1,IPOS)
               IF(IPOS.EQ.0)GOTO 30
               IALAD=IZGADR(LHNT,IPOS)-100000
               IALH=INT(IALAD/100)
               CALL IZSCAN(LHNT,ICHHCO,IADRI5,-1,IPOS)
               IF(IPOS.EQ.0)GOTO 30
               IACHH=IZGADR(LHNT,IPOS)
               TEXTHE=Q(LHF+IACHH)
   30          KATT=1
            ENDIF
            IF(IQ(LHI+IADRI2).EQ.2.AND.KATT.EQ.0)THEN
               IADRI5=ABS(IZGADR(LHNT,IQ(LHI+IADRI2+2)))
               ICHOPT=IQ(LHI+IADRI2+3)
               KATT=1
            ENDIF
            IADRI2=IQ(LHI+IADRI2+1)
   40    CONTINUE
         IADRI=IADRI2
         ICOD=ABS(ICOD)
      ENDIF
*
      IF(ICOD.EQ.ICHHCO)THEN
         TEXTHE=Q(LHF+IADRI)
      ENDIF
*
      IF(ICOD.EQ.IANGCO)THEN
      ENDIF
*
      IF(ICOD.EQ.ITXACO)THEN
         IALH=INT((IADRI-100000)/100)
      ENDIF
*______________ Polyline
      IF(ICOD.EQ.IPLCO)THEN
         IADRF=IQ(LHI+IADRI)
         N=IQ(LHI+IADRI+1)
         IF(DX.NE.0..OR.DY.NE.0.)THEN
            DO 50 I=0,N-1
               Q(LHF+IADRF+I)=Q(LHF+IADRF+I)+DX
               Q(LHF+IADRF+I+N)=Q(LHF+IADRF+I+N)+DY
   50       CONTINUE
         ENDIF
         IF(IZPKPL(PX,PY,N
     +,  Q(LHF+IADRF),Q(LHF+IADRF+N)).NE.0)THEN
            NPRIM=IREPTR
            IADRAT=IADRI5
         ENDIF
         IF(DX.NE.0..OR.DY.NE.0.)THEN
            DO 60 I=0,N-1
               Q(LHF+IADRF+I)=Q(LHF+IADRF+I)-DX
               Q(LHF+IADRF+I+N)=Q(LHF+IADRF+I+N)-DY
   60       CONTINUE
         ENDIF
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ Polyline with 2 points
      IF(ICOD.EQ.IPL2CO)THEN
         IADRF=IADRI
         X(1)=Q(LHF+IADRF)+DX
         X(2)=Q(LHF+IADRF+1)+DX
         Y(1)=Q(LHF+IADRF+2)+DY
         Y(2)=Q(LHF+IADRF+3)+DY
         IF(IZPKPL(PX,PY,2,X,Y).NE.0)THEN
            NPRIM=IREPTR
            IADRAT=IADRI5
         ENDIF
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ Polymarker
      IF(ICOD.EQ.IPMCO)THEN
         IADRF=IADRI
         S=1
         IF(REDIT.NE.0)THEN
            S=REDIT
         ENDIF
         DX=(S*0.01875*(RQUEST(21)-RQUEST(20)))/(RQUEST(31)-RQUEST(30))
         DY=(S*0.01875*(RQUEST(23)-RQUEST(22)))/(RQUEST(33)-RQUEST(32))
         DO 70 I=1,IQ(LHI+IADRI+1)
            XPMM=Q(LHF+IADRF+I-1)
            YPMM=Q(LHF+IADRF+I+N-1)
            IF(PX.GE.(XPMM-DX).AND.PX.LE.(XPMM+DX)
     +                       .AND.
     +         PY.GE.(YPMM-DY).AND.PY.LE.(YPMM+DY))THEN
               NPRIM=IREPTR
               IADRAT=IADRI5
               DX=0.
               DY=0.
               GOTO 10
            ENDIF
   70    CONTINUE
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ Axis
      IF(ICOD.EQ.IAXICO)THEN
         IADRF=IQ(LHI+IADRI)
         X(1)=Q(LHF+IADRF)+DX
         X(2)=Q(LHF+IADRF+1)+DX
         Y(1)=Q(LHF+IADRF+2)+DY
         Y(2)=Q(LHF+IADRF+3)+DY
         IF(IZPKPL(PX,PY,2,X,Y).NE.0)THEN
            NPRIM=IREPTR
            IADRAT=IADRI5
            IF(ICHOPT.NE.-1)THEN
               IACHOP=ICHOPT
            ELSE
               IACHOP=IQ(LHI+IADRI+2)
            ENDIF
         ENDIF
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ Fill area
      IF(ICOD.EQ.IFACO)THEN
         IADRF=IQ(LHI+IADRI)
         N=IQ(LHI+IADRI+1)
         IF(DX.NE.0..OR.DY.NE.0.)THEN
            DO 80 I=0,N-1
               Q(LHF+IADRF+I)=Q(LHF+IADRF+I)+DX
               Q(LHF+IADRF+I+N)=Q(LHF+IADRF+I+N)+DY
   80       CONTINUE
         ENDIF
         IF(IZPKFA(PX,PY,N
     +,  Q(LHF+IADRF),Q(LHF+IADRF+N)).NE.0)THEN
            NPRIM=IREPTR
            IADRAT=IADRI5
         ENDIF
         IF(DX.NE.0..OR.DY.NE.0.)THEN
            DO 90 I=0,N-1
               Q(LHF+IADRF+I)=Q(LHF+IADRF+I)-DX
               Q(LHF+IADRF+I+N)=Q(LHF+IADRF+I+N)-DY
   90       CONTINUE
         ENDIF
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ Boxes
      IF(ICOD.EQ.IBXCO)THEN
         IADRF=IADRI
         X(1)=Q(LHF+IADRF)+DX
         X(2)=Q(LHF+IADRF)+DX
         X(3)=Q(LHF+IADRF+1)+DX
         X(4)=Q(LHF+IADRF+1)+DX
         Y(1)=Q(LHF+IADRF+2)+DY
         Y(2)=Q(LHF+IADRF+3)+DY
         Y(3)=Q(LHF+IADRF+3)+DY
         Y(4)=Q(LHF+IADRF+2)+DY
         IF(IZPKFA(PX,PY,4,X,Y).NE.0)THEN
            NPRIM=IREPTR
            IADRAT=IADRI5
         ENDIF
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ Text
      IF(ICOD.EQ.ITXCO)THEN
         IADRF=IQ(LHI+IADRI)
         NCHP=IQ(LHI+IADRI+2)
         X(1)=Q(LHF+IADRF)+DX
         Y(1)=Q(LHF+IADRF+1)+DY
         IF(IALH.EQ.2)THEN
            X(1)=Q(LHF+IADRF)-((NCHP/2)*TEXTHE)+DX
         ENDIF
         IF(IALH.EQ.3)THEN
            X(1)=Q(LHF+IADRF)-(NCHP*TEXTHE)+DX
         ENDIF
         X(2)=X(1)
         Y(2)=Y(1)+TEXTHE
         X(3)=X(1)+NCHP*TEXTHE
         Y(3)=Y(2)
         X(4)=X(3)
         Y(4)=Y(1)
         IF(IZPKFA(PX,PY,4,X,Y).NE.0)THEN
            NPRIM=IREPTR
            IADRAT=IADRI5
         ENDIF
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ IGTEXT
      IF(ICOD.EQ.IGTXCO)THEN
         IF(ICHOPT.LT.0)THEN
            ICHOPT=IQ(LHI+IADRI+3)
         ENDIF
         IADRF=IQ(LHI+IADRI)
         NCHP=IQ(LHI+IADRI+2)
         SSIZE=Q(LHF+IADRF+2)
         X(1)=Q(LHF+IADRF)+DX
         Y(1)=Q(LHF+IADRF+1)+DY
         IF(JBIT(ICHOPT,2).NE.0)THEN
            X(1)=Q(LHF+IADRF)-((NCHP/2)*SSIZE)+DX
         ENDIF
         IF(JBIT(ICHOPT,3).NE.0)THEN
            X(1)=Q(LHF+IADRF)-(NCHP*SSIZE)+DX
         ENDIF
         X(2)=X(1)
         Y(2)=Y(1)+SSIZE
         X(3)=X(1)+NCHP*SSIZE
         Y(3)=Y(2)
         X(4)=X(3)
         Y(4)=Y(1)
         IF(IZPKFA(PX,PY,4,X,Y).NE.0)THEN
            NPRIM=IREPTR
            IADRAT=IADRI5
            IACHOP=ICHOPT
         ENDIF
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ Frame boxes
      IF(ICOD.EQ.IFBXCO)THEN
         IADRF=IADRI
         X(1)=Q(LHF+IADRF)+DX
         X(2)=Q(LHF+IADRF)+DX
         X(3)=Q(LHF+IADRF+1)+DX
         X(4)=Q(LHF+IADRF+1)+DX
         X(5)=Q(LHF+IADRF)+DX
         X(6)=Q(LHF+IADRF+4)+DX
         X(7)=Q(LHF+IADRF+5)+DX
         X(8)=Q(LHF+IADRF+5)+DX
         X(9)=Q(LHF+IADRF+4)+DX
         X(10)=Q(LHF+IADRF+4)+DX
         Y(1)=Q(LHF+IADRF+2)+DY
         Y(2)=Q(LHF+IADRF+3)+DY
         Y(3)=Q(LHF+IADRF+3)+DY
         Y(4)=Q(LHF+IADRF+2)+DY
         Y(5)=Q(LHF+IADRF+2)+DY
         Y(6)=Q(LHF+IADRF+6)+DY
         Y(7)=Q(LHF+IADRF+6)+DY
         Y(8)=Q(LHF+IADRF+7)+DY
         Y(9)=Q(LHF+IADRF+7)+DY
         Y(10)=Q(LHF+IADRF+6)+DY
         IF(IZPKFA(PX,PY,10,X,Y).NE.0)THEN
            NPRIM=IREPTR
            IADRAT=IADRI5
         ENDIF
         DX=0.
         DY=0.
         GOTO 10
      ENDIF
*______________ Histograms
      IF(ICOD.EQ.IHISCO)THEN
         IADRF=IQ(LHI+IADRI)
         NN=2
         IF(JBIT(IQ(LHI+IADRI+2),8).NE.0)THEN
            NN=IQ(LHI+IADRI+1)+1
         ENDIF
         IF(JBIT(IQ(LHI+IADRI+2),5).NE.0)THEN
C            CALL IGHIST(IQ(LIDECO+IADRI+1)
C     +,               Q(LFDECO+IADRF+NN)
C     +,               Q(LFDECO+IADRF)
C     +,               CHOPT)
            DX=0.
            DY=0.
            GOTO 10
         ELSE
C         CALL IGHIST(IQ(LIDECO+IADRI+1)
C     +,            Q(LFDECO+IADRF)
C     +,            Q(LFDECO+IADRF+NN)
C     +,            CHOPT)
            IF(PX.GE.Q(LHF+IADRF).AND.PX.LE.Q(LHF+IADRF+NN-1))THEN
               XINT=(Q(LHF+IADRF+NN-1)-Q(LHF+IADRF)) /IQ(LHI+IADRI+
     +         1)
               INDEX=(PX-Q(LHF+IADRF))/XINT
               IF(Q(LHF+IADRF+NN+INDEX).GE.PY)THEN
                  NPRIM=IREPTR
                  IADRAT=IADRI5
                  IF(ICHOPT.NE.-1)THEN
                     IACHOP=ICHOPT
                  ELSE
                     IACHOP=IQ(LHI+IADRI+2)
                  ENDIF
               ENDIF
            ENDIF
            DX=0.
            DY=0.
            GOTO 10
         ENDIF
      ENDIF
      DX=0.
      DY=0.
      GOTO 10
*
  100 CALL IZSCPI(LPSAV)
*
      END
