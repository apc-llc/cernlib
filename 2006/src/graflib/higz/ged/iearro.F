*
* $Id: iearro.F,v 1.1.1.1 1996/02/14 13:10:21 mclareni Exp $
*
* $Log: iearro.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.47.23  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEARRO(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
#include "higz/hiatt.inc"
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*32 CHVAL(15)
      CHARACTER*12 CHTEMP
      DIMENSION UU(3),VV(3),UU1(3),VV1(3)
*.______________________________________
*
      ICHOIO=0
   10 IPLACE=2
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      NTSAV=INTR
      CALL ISELNT(1)
      CALL IGREQ(21,3,16,IPLACE,ICHOIC,CHVAL)
      IF(ICHOIC.EQ.-1000)GOTO 10
*
   20 X(1)=RQUEST(13)
      Y(1)=RQUEST(14)
      CALL IGGRID(X(1),Y(1))
      NTLOC=IQUEST(10)
*
   40 IF(IPLACE.EQ.1)THEN
         ICF=1
         ICHOIO=ICHOIC
         CALL ISELNT(NTSAV)
         RETURN
      ENDIF
*
      IF(IPLACE.EQ.3)GOTO 10
*
      IF(ICHOIC.EQ.-1)THEN
         TITLE(3)='ARROW ATTRIBUTES'
         CHITEM(1,3)='Arrow size'
         CHITEM(2,3)='Double arrow'
         CHITEM(3,3)='Filled arrow'
         CHITEM(4,3)='Fill area color'
         CHITEM(5,3)='Line color'
         CHITEM(6,3)='Fill interior style'
         CHITEM(7,3)='Fill style index'
         CHITEM(8,3)='Line type'
         CALL IZRTOC(RCARSI,CHDEF(1))
         CHDEF(2)=CHDOAR
         CHDEF(3)=CHAFL
         CHDEF(4)=CHARFC
         CHDEF(5)=CHARLC
         CHDEF(6)=CHARFI
         CALL IZITOC(ICARFS,CHDEF(7))
         CALL IZITOC(ICARLN,CHDEF(8))
         IPLACE=3
         CALL ISELNT(1)
         CALL IGREQ(299,0,8,IPLACE,ICHOIC,CHVAL)
         ICHOIO=ICHOIC
         ICF=IPLACE
         CALL IZCTOR(CHVAL(1),RCARSI)
         CALL IGFIRS(CHDOAR,CHVAL(2))
         CALL IGFIRS(CHAFL,CHVAL(3))
         CALL IGFIRS(CHARFC,CHVAL(4))
         CALL IGFIRS(CHARLC,CHVAL(5))
         CALL IGFIRS(CHARFI,CHVAL(6))
         ICARFC=IGICOL(CHVAL(4))
         ICARLC=IGICOL(CHVAL(5))
         ICARFI=IGIFAI(CHVAL(6))
         CHTEMP=CHVAL(7)
         CALL IZCTOI(CHTEMP,ICARFS)
         CHTEMP=CHVAL(8)
         CALL IZCTOI(CHTEMP,ICARLN)
         GOTO 20
      ENDIF
*
      IF(ICHOIC.EQ.-2)THEN
         CALL IGCLES
         CALL IZPICT(EDIPIC,'D')
         CALL IEGRID
         GOTO 10
      ENDIF
*
      IF(ICHOIC.EQ.-3)THEN
         CALL IZUNDO
         GOTO 10
      ENDIF
*
      IF(ICHOIC.GE.1)THEN
         ICF=2
         ICHOIO=ICHOIC
         CALL ISELNT(NTSAV)
         RETURN
      ENDIF
*
      I=2
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      IF(NTLOC.NE.1)THEN
         I=1
      ENDIF
      CALL ISELNT(1)
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL IGPLOT(X(1),Y(1))
   50 CALL IGLOC(41,NTLOC,IBN,XNDC,YNDC,X(I),Y(I))
      IF(IBN.EQ.1)THEN
         IF(NTLOC.NE.1)GOTO 50
         IF(I.EQ.1)GOTO 51
         CALL IGGRID(X(I),Y(I))
         CALL IEALPT(X(I-1),Y(I-1))
         CALL IGLINE(X(I-1),X(I),Y(I-1),Y(I))
   51    I=I+1
         GOTO 50
      ENDIF
*
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL IZSAV
      CALL ISPLCI(0)
      CALL ISLN(1)
      CALL IPL(I-1,X,Y)
      CALL IZSET
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISPLCI(ICARLC)
      CALL ISFACI(ICARFC)
      CALL ISFAIS(ICARFI)
      CALL ISFASI(ICARFS)
      CALL ISLN(ICARLN)
      IF(RCARSI.LE.0.)GOTO 10
      TGAR=0.6
      FSIN=0.
      FCOS=1.
      XL=SQRT((X(I-1)-X(I-2))**2 + (Y(I-1)-Y(I-2))**2)
      IF(XL.GT.0.)THEN
         FSIN=(Y(I-1)-Y(I-2))/XL
         FCOS=(X(I-1)-X(I-2))/XL
      ENDIF
      UU(1)=X(I-2)+(XL-RCARSI)*FCOS-RCARSI*TGAR*FSIN
      UU(3)=X(I-2)+(XL-RCARSI)*FCOS+RCARSI*TGAR*FSIN
      VV(1)=Y(I-2)+(XL-RCARSI)*FSIN+RCARSI*TGAR*FCOS
      VV(3)=Y(I-2)+(XL-RCARSI)*FSIN-RCARSI*TGAR*FCOS
      UU(2)=X(I-1)
      VV(2)=Y(I-1)
      IF(INDEX(CHVAL(3),'Y').NE.0)THEN
         CALL IFA(3,UU,VV)
      ELSE
         CALL IPL(3,UU,VV)
      ENDIF
      IF(INDEX(CHVAL(2),'Y').NE.0)THEN
         TGAR=0.6
         FSIN=0.
         FCOS=1.
         XL=SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2)
         IF(XL.GT.0.)THEN
            FSIN=(Y(1)-Y(2))/XL
            FCOS=(X(1)-X(2))/XL
         ENDIF
         UU1(1)=X(2)+(XL-RCARSI)*FCOS-RCARSI*TGAR*FSIN
         UU1(3)=X(2)+(XL-RCARSI)*FCOS+RCARSI*TGAR*FSIN
         VV1(1)=Y(2)+(XL-RCARSI)*FSIN+RCARSI*TGAR*FCOS
         VV1(3)=Y(2)+(XL-RCARSI)*FSIN-RCARSI*TGAR*FCOS
         UU1(2)=X(1)
         VV1(2)=Y(1)
         IF(INDEX(CHVAL(3),'Y').NE.0)THEN
            CALL IFA(3,UU1,VV1)
         ELSE
            CALL IPL(3,UU1,VV1)
         ENDIF
      ENDIF
      IF(INDEX(CHVAL(3),'Y').NE.0)THEN
         X(I-1)=(UU(3)+UU(1))/2.
         Y(I-1)=(VV(3)+VV(1))/2.
         IF(INDEX(CHVAL(2),'Y').NE.0)THEN
            X(1)=(UU1(3)+UU1(1))/2.
            Y(1)=(VV1(3)+VV1(1))/2.
         ENDIF
      ENDIF
      CALL IPL(I-1,X,Y)
   70 CONTINUE
      GOTO 10
*
      END
