*
* $Id: izdnb.F,v 1.1.1.1 1996/02/14 13:11:09 mclareni Exp $
*
* $Log: izdnb.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:09  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.23/02 30/08/95  18.19.28  by  O.Couet
*-- Author :
      SUBROUTINE IZDNB(LN,LI,LF,LC,IADRES,IALON)
*.===========>
*.
*.   This routine Decode the NT Bank referenced by LN.
*.
*. _Input parameters:
*.
*.  INTEGER LN : NT bank link in the ZEBRA data structure .
*.  INTEGER LI : LHI bank link in the ZEBRA data structure .
*.  INTEGER LF : LHF bank link in the ZEBRA data structure .
*.  INTEGER LC : LHC bank link in the ZEBRA data structure .
*.  INTEGER IADRES : Adress to be decode if IALON=1.
*.  INTEGER IALON : =0 the whole bank is decoded ;
*.                  =1 only the code at the adress IADRES is decoded
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatnb.inc"
#include "higz/hiatt.inc"
#include "higz/hilabs.inc"
      CHARACTER*256 CHARS,STR
      CHARACTER*80 CHOPT
      CHARACTER*1 CHAXIS(25),CHHIST(14),CHTEXT(3),CHTABL(11),CHGRAP(12)
     +,           CHPAVE(9)
      DIMENSION X(2),Y(2)
#include "higz/hiatnm.inc"
      DATA CHAXIS /'G','B','A','V','+','-','U','P','O'
     +,            'R','L','C','=','.','H','D','Y','T'
     +,            'W','S','N','I','M','0','X'/
      DATA CHHIST /'H','F','C','*','R','1','B','N','L'
     +,            'P','A','G','X','Y'/
      DATA CHTEXT /'L','C','R'/
      DATA CHTABL /'P','B','C','T','K','L','S',' ','A','+','R'/
      DATA CHGRAP /'L','A','C','*','P','B','R','1','G','X','Y','F'/
      DATA CHPAVE /'T','B','L','R','-','S','P','K','D'/
*.______________________________________
*
      LNDECO=LN
      LIDECO=LI
      LFDECO=LF
      LCDECO=LC
*
      IF(IALON.NE.0)THEN
         IREPTR=IADRES
         IREPTR=IREPTR-1
         GOTO 1
      ENDIF
*
      IREPTR=9
      IDIAA=IZGADR(LNDECO,8)
      IDRAA=IZGADR(LNDECO,9)
      INBIAT=IZGCOD(LNDECO,8)
      INBRAT=IZGCOD(LNDECO,9)
*
*              Set the default attributes
*
      DO 3 I=1,INBIAT
         CALL IGSET(CHIATT(I),FLOAT(IQ(LIDECO+IDIAA+I-1)))
 3    CONTINUE
      DO 4 I=1,INBRAT
         CALL IGSET(CHRATT(I),Q(LFDECO+IDRAA+I-1))
 4    CONTINUE
*
*              Decode the LN bank
*
    1 IREPTR=IREPTR+1
      IF(IREPTR.EQ.IQ(LNDECO+4))RETURN
      IADRI=ABS(IZGADR(LNDECO,IREPTR))
      ICODE=IZGCOD(LNDECO,IREPTR)
*
      GOTO (2,2,2,2,2
     +,     60,70,80,90,100,110,120,130,140,150,160,170,180,190
     +,     200,210,220,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
     +,     2,480,490
     +,     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
     +,     8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
     +      ),ABS(ICODE)
*
    2 CONTINUE
      IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Histograms
*
   60 IADRI2=IADRI
      IF(ICODE.GT.0)THEN
         ICHOPT=IQ(LIDECO+IADRI+2)
      ELSE
         ICHOPT=-1
      ENDIF
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
   61    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 61
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 62 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 63
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.2).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
                  ICHOPT=IQ(LIDECO+IADRI2+3)
               ENDIF
               KATT=1
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
   62    CONTINUE
      ENDIF
*
      IADRF=IQ(LIDECO+IADRI2)
      IF(ICHOPT.LT.0)THEN
         ICHOPT=IQ(LIDECO+IADRI2+2)
      ENDIF
      CHOPT=' '
      DO 64 I=1,14
         IF(JBIT(ICHOPT,I).NE.0)CHOPT(I:I)=CHHIST(I)
   64 CONTINUE
      NN=2
      IF(JBIT(ICHOPT,8).NE.0)THEN
         NN=IQ(LIDECO+IADRI2+1)+1
      ENDIF
      IF(JBIT(ICHOPT,5).NE.0)THEN
         IQUEST(81)   = LFDECO+IADRF+NN
         IQUEST(82)   = LFDECO+IADRF
         CHOPT(20:20) = 'Z'
         CALL IGHIST(IQ(LIDECO+IADRI2+1)
     +,               Q(LFDECO+IADRF+NN)
     +,               Q(LFDECO+IADRF)
     +,               CHOPT)
         GOTO 1
      ENDIF
      NBIN=IQ(LIDECO+IADRI2+1)
      IF(IZPUSH(3,NBIN+2,0,'IZDNB').NE.0)RETURN
      IQUEST(81)   = LFDECO+IADRF
      IQUEST(82)   = LFDECO+IADRF+NN
      CHOPT(20:20) = 'Z'
      CALL IGHIST(NBIN
     +,            Q(LFDECO+IADRF)
     +,            Q(LFDECO+IADRF+NN)
     +,            CHOPT)
      IF(ICODE.LT.0)CALL IZSETA
*
   63 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Polymarker with one point
*
   70 IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
   71    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 71
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 72 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 73
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
   72    CONTINUE
      ENDIF
*
      IF(IZPUSH(2,2,0,'IZDNB').NE.0)RETURN
      CALL IPM(1,Q(LFDECO+IADRI2),Q(LFDECO+IADRI2+1))
      IF(ICODE.LT.0)CALL IZSETA
*
   73 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Polyline with two points
*
   80 DX=0.
      DY=0.
      IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
   81    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 81
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 82 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 83
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
   82    CONTINUE
      ENDIF
*
      X(1)=Q(LFDECO+IADRI2)+DX
      X(2)=Q(LFDECO+IADRI2+1)+DX
      Y(1)=Q(LFDECO+IADRI2+2)+DY
      Y(2)=Q(LFDECO+IADRI2+3)+DY
      CALL IPL(2,X,Y)
      IF(ICODE.LT.0)CALL IZSETA
*
   83 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Polyline
*
   90 DX=0.
      DY=0.
      IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
   91    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 91
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 92 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 93
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
   92    CONTINUE
      ENDIF
*
      IADRF=IQ(LIDECO+IADRI2)
      N=IQ(LIDECO+IADRI2+1)
      IF(DX.NE.0..OR.DY.NE.0.)THEN
         DO 94 I=0,N-1
            Q(LFDECO+IADRF+I)=Q(LFDECO+IADRF+I)+DX
            Q(LFDECO+IADRF+I+N)=Q(LFDECO+IADRF+I+N)+DY
   94    CONTINUE
      ENDIF
      IF(IZPUSH(2,2*N,0,'IZDNB').NE.0)RETURN
      CALL IPL(N
     +,        Q(LFDECO+IADRF)
     +,        Q(LFDECO+IADRF+N))
      IF(DX.NE.0..OR.DY.NE.0.)THEN
         DO 95 I=0,N-1
            Q(LFDECO+IADRF+I)=Q(LFDECO+IADRF+I)-DX
            Q(LFDECO+IADRF+I+N)=Q(LFDECO+IADRF+I+N)-DY
   95    CONTINUE
      ENDIF
      IF(ICODE.LT.0)CALL IZSETA
*
   93 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Polymarker
*
  100 IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  101    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 101
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 102 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 103
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  102    CONTINUE
      ENDIF
*
      IADRF=IQ(LIDECO+IADRI2)
      N=IQ(LIDECO+IADRI2+1)
      IF(IZPUSH(2,2*N,0,'IZDNB').NE.0)RETURN
      CALL IPM(N
     +,        Q(LFDECO+IADRF)
     +,        Q(LFDECO+IADRF+N))
      IF(ICODE.LT.0)CALL IZSETA
*
  103 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Fill area
*
  110 DX=0.
      DY=0.
      IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  111    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 111
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 112 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 113
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  112    CONTINUE
      ENDIF
*
      IADRF=IQ(LIDECO+IADRI2)
      N=IQ(LIDECO+IADRI2+1)
      IF(DX.NE.0..OR.DY.NE.0.)THEN
         DO 114 I=0,N-1
            Q(LFDECO+IADRF+I)=Q(LFDECO+IADRF+I)+DX
            Q(LFDECO+IADRF+I+N)=Q(LFDECO+IADRF+I+N)+DY
  114    CONTINUE
      ENDIF
      IF(IZPUSH(2,2*N,0,'IZDNB').NE.0)RETURN
      CALL IFA(N
     +,        Q(LFDECO+IADRF)
     +,        Q(LFDECO+IADRF+N))
      IF(DX.NE.0..OR.DY.NE.0.)THEN
         DO 115 I=0,N-1
            Q(LFDECO+IADRF+I)=Q(LFDECO+IADRF+I)-DX
            Q(LFDECO+IADRF+I+N)=Q(LFDECO+IADRF+I+N)-DY
  115    CONTINUE
      ENDIF
      IF(ICODE.LT.0)CALL IZSETA
*
  113 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Text
*
  120 DX=0.
      DY=0.
      IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  121    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 121
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 122 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 123
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  122    CONTINUE
      ENDIF
*
      IADRF=IQ(LIDECO+IADRI2)
      IADRC=IQ(LIDECO+IADRI2+1)
      NCHP=IQ(LIDECO+IADRI2+2)
      CALL UHTOC(IQ(LCDECO+IADRC),4,STR,NCHP)
      CHARS=STR(1:NCHP)
      IF(IZPUSH(3,2,(NCHP+3)/4,'IZDNB').NE.0)RETURN
      CALL ITX(Q(LFDECO+IADRF)+DX
     +,        Q(LFDECO+IADRF+1)+DY
     +,        CHARS)
      IF(ICODE.LT.0)CALL IZSETA
*
  123 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Boxe
*
  130 DX=0.
      DY=0.
      IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  131    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 131
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 132 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 133
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  132    CONTINUE
      ENDIF
*
      IF(IZPUSH(0,4,0,'IZDNB').NE.0)RETURN
      CALL IGBOX(Q(LFDECO+IADRI2)+DX
     +,          Q(LFDECO+IADRI2+1)+DX
     +,          Q(LFDECO+IADRI2+2)+DY
     +,          Q(LFDECO+IADRI2+3)+DY)
      IF(ICODE.LT.0)CALL IZSETA
*
  133 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Frame box
*
  140 DX=0.
      DY=0.
      IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  141    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 141
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 142 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 143
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  142    CONTINUE
      ENDIF
*
      IF(IZPUSH(0,8,0,'IZDNB').NE.0)RETURN
      CALL IGFBOX(Q(LFDECO+IADRI2)+DX
     +,           Q(LFDECO+IADRI2+1)+DX
     +,           Q(LFDECO+IADRI2+2)+DY
     +,           Q(LFDECO+IADRI2+3)+DY
     +,           Q(LFDECO+IADRI2+4)+DX
     +,           Q(LFDECO+IADRI2+5)+DX
     +,           Q(LFDECO+IADRI2+6)+DY
     +,           Q(LFDECO+IADRI2+7)+DY)
      IF(ICODE.LT.0)CALL IZSETA
*
  143 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Arc
*
  150 DX=0.
      DY=0.
      IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  151    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 151
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 152 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 153
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  152    CONTINUE
      ENDIF
*
      IF(IZPUSH(0,6,0,'IZDNB').NE.0)RETURN
      CALL IGARC(Q(LFDECO+IADRI2)+DX
     +,          Q(LFDECO+IADRI2+1)+DY
     +,          Q(LFDECO+IADRI2+2)
     +,          Q(LFDECO+IADRI2+3)
     +,          Q(LFDECO+IADRI2+4)
     +,          Q(LFDECO+IADRI2+5))
      IF(ICODE.LT.0)CALL IZSETA
*
  153 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Axis
*
  160 DX=0.
      DY=0.
      IADRI2=IADRI
      IF(ICODE.GT.0)THEN
         ICHOPT=IQ(LIDECO+IADRI+2)
      ELSE
         ICHOPT=-1
      ENDIF
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  161    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 161
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 162 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 163
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.2).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
                  ICHOPT=IQ(LIDECO+IADRI2+3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  162    CONTINUE
      ENDIF
*
      IADRF=IQ(LIDECO+IADRI2)
      IF(ICHOPT.LT.0)THEN
         ICHOPT=IQ(LIDECO+IADRI2+2)
      ENDIF
      CHOPT=' '
      DO 164 I=1,25
         IF(JBIT(ICHOPT,I).NE.0)CHOPT(I:I)=CHAXIS(I)
  164 CONTINUE
      IF(IZPUSH(3,6,0,'IZDNB').NE.0)RETURN
      CALL IGAXIS(Q(LFDECO+IADRF)+DX
     +,           Q(LFDECO+IADRF+1)+DX
     +,           Q(LFDECO+IADRF+2)+DY
     +,           Q(LFDECO+IADRF+3)+DY
     +,           Q(LFDECO+IADRF+4)
     +,           Q(LFDECO+IADRF+5)
     +,          IQ(LIDECO+IADRI2+1)
     +,           CHOPT)
      IF(ICODE.LT.0)CALL IZSETA
*
  163 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Software characters
*
  170 DX=0.
      DY=0.
      IADRI2=IADRI
      IF(ICODE.GT.0)THEN
         ICHOPT=IQ(LIDECO+IADRI+3)
      ELSE
         ICHOPT=-1
      ENDIF
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  171    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 171
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 172 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 173
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.2).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
                  ICHOPT=IQ(LIDECO+IADRI2+3)
               ENDIF
               KATT=1
            ENDIF
            IF(IZGCOD(LIDECO,IADRI2).EQ.3)THEN
               IADRI4=IZGADR(LIDECO,IADRI2)
               DX=DX+Q(LFDECO+IADRI4)
               DY=DY+Q(LFDECO+IADRI4+1)
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  172    CONTINUE
      ENDIF
*
      IADRF=IQ(LIDECO+IADRI2)
      IADRC=IQ(LIDECO+IADRI2+1)
      NCHP=IQ(LIDECO+IADRI2+2)
      IF(ICHOPT.LT.0)THEN
         ICHOPT=IQ(LIDECO+IADRI2+3)
      ENDIF
      CHOPT=' '
      DO 174 I=1,3
         IF(JBIT(ICHOPT,I).NE.0)CHOPT(I:I)=CHTEXT(I)
  174 CONTINUE
      CALL UHTOC(IQ(LCDECO+IADRC),4,STR,NCHP)
      CHARS=STR(1:NCHP)
      IF(IZPUSH(4,4,(NCHP+3)/4,'IZDNB').NE.0)RETURN
      CALL IGTEXT(Q(LFDECO+IADRF)+DX
     +,           Q(LFDECO+IADRF+1)+DY
     +,           CHARS
     +,           Q(LFDECO+IADRF+2)
     +,           Q(LFDECO+IADRF+3)
     +,           CHOPT)
      IF(ICODE.LT.0)CALL IZSETA
*
  173 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Multiline
*
  180 IADRI2=IADRI
*
      IF(ICODE.LT.0)THEN
         CALL IZSAVA
         NBNOP=1
         KATT=0
  181    IF(IQ(LNDECO+IADRI2).LT.0)THEN
            NBNOP=NBNOP+1
            IADRI2=ABS(IZGADR(LNDECO,IADRI2))
            GOTO 181
         ENDIF
         IADRI2=IZGADR(LNDECO,IADRI2)
         DO 182 I=1,NBNOP
            IF(IQ(LIDECO+IADRI2).EQ.0)THEN
               GOTO 183
            ENDIF
            IF((IQ(LIDECO+IADRI2).EQ.1).AND.(KATT.EQ.0))THEN
               IF(IALON.NE.-1)THEN
                  IADRI3=ABS(IZGADR(LNDECO,IQ(LIDECO+IADRI2+2)))-1
                  CALL IZDATT(IREPTR+1,IADRI3)
               ENDIF
               KATT=1
            ENDIF
            IADRI2=IQ(LIDECO+IADRI2+1)
  182    CONTINUE
      ENDIF
*
      IADRF=IQ(LIDECO+IADRI2)
      N=IQ(LIDECO+IADRI2+1)
      IF(IZPUSH(2,2*N,0,'IZDNB').NE.0)RETURN
      CALL IML(N
     +,        Q(LFDECO+IADRF)
     +,        Q(LFDECO+IADRF+N))
      IF(ICODE.LT.0)CALL IZSETA
*
  183 IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Table
*
  200 NX=IQ(LIDECO+IADRI)
      NY=IQ(LIDECO+IADRI+1)
      NYY=MAX(NY,1)
      IADRF=IQ(LIDECO+IADRI+2)
      NPAR=IQ(LIDECO+IADRI+3)
      ICHOPT=IQ(LIDECO+IADRI+4)
      CHOPT=' '
      IF(JBIT(ICHOPT, 1).NE.0)CHOPT( 1: 3)='COL'
      IF(JBIT(ICHOPT,13).NE.0)CHOPT(16:17)='S1'
      IF(JBIT(ICHOPT,14).NE.0)CHOPT(16:17)='S2'
      IF(JBIT(ICHOPT,15).NE.0)CHOPT(16:17)='L1'
      IF(JBIT(ICHOPT,16).NE.0)CHOPT(16:17)='L2'
      IF(JBIT(ICHOPT,17).NE.0)CHOPT(18:19)='GZ'
      IF(JBIT(ICHOPT,18).NE.0)CHOPT(20:21)='GX'
      IF(JBIT(ICHOPT,19).NE.0)CHOPT(22:23)='GY'
      IF(JBIT(ICHOPT,20).NE.0)CHOPT(15:16)='LB'
      IF(JBIT(ICHOPT,21).NE.0)CHOPT( 4: 4)='Z'
      IF(JBIT(ICHOPT,22).NE.0)CHOPT(16:17)='S3'
      IF(JBIT(ICHOPT,23).NE.0)CHOPT(16:17)='S4'
      IF(JBIT(ICHOPT,24).NE.0)CHOPT( 1: 3)='POL'
      IF(JBIT(ICHOPT,25).NE.0)CHOPT( 1: 3)='CYL'
      IF(JBIT(ICHOPT,26).NE.0)CHOPT( 1: 3)='SPH'
      IF(JBIT(ICHOPT,27).NE.0)CHOPT( 1: 3)='PSD'
      IF(JBIT(ICHOPT,28).NE.0)CHOPT(38:39)='BB'
      IF(JBIT(ICHOPT,29).NE.0)CHOPT(36:37)='FB'
      IF(JBIT(ICHOPT,30).NE.0)CHOPT(41:41)='E'
      DO 201 I=2,12
         IF(JBIT(ICHOPT,I).NE.0)CHOPT(I+2:I+2)=CHTABL(I-1)
  201 CONTINUE
      CHOPT(40:40) = 'I'
*
      NPAR2=NPAR
      IF(NPAR.GE.9)THEN
         IF(Q(LFDECO+IADRF+8).LT.0.)THEN
            IQUEST(60)=Q(LFDECO+IADRF+NPAR)
            NPAR2=NPAR+2
         ENDIF
         IF(Q(LFDECO+IADRF+9).LT.0.)THEN
            IQUEST(61)=Q(LFDECO+IADRF+NPAR+1)
            NPAR2=NPAR+2
         ENDIF
      ENDIF
*
      IF(IZPUSH(5,NX*NYY+NPAR,0,'IZDNB').NE.0)RETURN
      CALL IGTABL(NX
     +,           NY
     +,           FLOAT(LFDECO+IADRF+NPAR2-1)
     +,           NPAR
     +,           Q(LFDECO+IADRF)
     +,           CHOPT)
*
      IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Graph
*
  210 IADRF  = IQ(LIDECO+IADRI)
      N      = IQ(LIDECO+IADRI+1)
      ICHOPT = IQ(LIDECO+IADRI+2)
      CHOPT  = ' '
      DO 211 I=1,12
         IF(JBIT(ICHOPT,I).NE.0)CHOPT(I:I)=CHGRAP(I)
  211 CONTINUE
*
      IF(IZPUSH(3,2*N,0,'IZDNB').NE.0)RETURN
      IQUEST(81)   = LFDECO+IADRF
      IQUEST(82)   = LFDECO+IADRF+N
      CHOPT(20:20) = 'Z'
      CALL IGRAPH(N
     +,           Q(LFDECO+IADRF)
     +,           Q(LFDECO+IADRF+N)
     +,           CHOPT)
*
      IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Pave
*
  220 IADRF  = IQ(LIDECO+IADRI)
      ICHOPT = IQ(LIDECO+IADRI+3)
      CHOPT  = ' '
      DO 221 I=1,9
         IF(JBIT(ICHOPT,I).NE.0)CHOPT(I:I)=CHPAVE(I)
  221 CONTINUE
*
      IF(IZPUSH(4,5,0,'IZDNB').NE.0)RETURN
      CALL IGPAVE(Q(LFDECO+IADRF)
     +,           Q(LFDECO+IADRF+1)
     +,           Q(LFDECO+IADRF+2)
     +,           Q(LFDECO+IADRF+3)
     +,           Q(LFDECO+IADRF+4)
     +,           IQ(LIDECO+IADRI+1)
     +,           IQ(LIDECO+IADRI+2)
     +,           CHOPT)
*
      IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Alpha numerique axis labels
*
  190 NHILAB=IQ(LIDECO+IADRI)
      IADRC=IQ(LIDECO+IADRI+1)
      LBLLEN=0
      DO 191 I=1,NHILAB
         NCHP=IQ(IADRI+LIDECO+I+1)
         HILABS(I)=' '
         CALL UHTOC(IQ(LCDECO+IADRC+LBLLEN),4,HILABS(I),NCHP)
         LBLLEN=LBLLEN+((NCHP+3)/4)
  191 CONTINUE
      CALL IZLBL
      IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Set color representation
*
  480 IADRF=IQ(LIDECO+IADRI)
      IF(IZPUSH(2,3,0,'IZDNB').NE.0)RETURN
      CALL ISCR(IDID
     +,         IQ(LIDECO+IADRI+1)
     +,         Q(LFDECO+IADRF)
     +,         Q(LFDECO+IADRF+1)
     +,         Q(LFDECO+IADRF+2))
      IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Set clipping indicator
*
  490 CALL ISCLIP(IADRI)
      IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Real attributes
*
  5   IF(IZPUSH(0,1,0,'IZDNB').NE.0)RETURN
      CALL IGSET(CHRATT(ICODE-49),Q(LFDECO+IADRI))
      IF(IALON.NE.0)RETURN
      GOTO 1
*
*              Integer attributes
*
  8   IF(IZPUSH(0,0,0,'IZDNB').NE.0)RETURN
      CALL IGSET(CHIATT(ICODE-79),FLOAT(IADRI-100000))
      IF(IALON.NE.0)RETURN
      GOTO 1
*
      END
#endif
