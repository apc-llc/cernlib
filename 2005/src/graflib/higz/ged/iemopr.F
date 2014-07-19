*
* $Id: iemopr.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iemopr.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.47.25  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEMOPR(ICF,ICHOIO)
*.===========>
*.
*. Move an existing primitive
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/hicode.inc"
#include "higz/higed.inc"
      CHARACTER*8 SNAME
      CHARACTER*4 CHVAL(15)
*.______________________________________
*
      ICHOIO=0
   10 IPLACE=2
      CALL IGREQ(21,3,16,IPLACE,ICHOIC,CHVAL)
      IF(IPLACE.EQ.3)GOTO 10
      IF(IPLACE.EQ.1)THEN
         ICF=1
         ICHOIO=ICHOIC
         RETURN
      ENDIF
      IF(ICHOIC.EQ.-1000)GOTO 10
      IF(ICHOIC.EQ.-1)GOTO 10
      IF(ICHOIC.EQ.-2)THEN
         CALL IGCLES
         CALL IZPICT(EDIPIC,'D')
         CALL IEGRID
         GOTO 10
      ENDIF
      IF(ICHOIC.EQ.-3)THEN
         CALL IZUNDO
         GOTO 10
      ENDIF
      IF(ICHOIC.GE.1)THEN
         ICF=2
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL IZPICK(NT,SNAME,NPRIM,'N')
      IF(NPRIM.LE.0)GOTO 50
*
      LN=IZGNTP(NT)
      IF(LN.EQ.0)GOTO 50
      CALL ISELNT(NT)
      X(1)=RQUEST(13)
      Y(1)=RQUEST(14)
      CALL IGGRID(X(1),Y(1))
   30 CALL IGLOC(41,NTLOC,IBN,XNDC,YNDC,X(2),Y(2))
      IF(IBN.EQ.0)GOTO 50
      IF(NTLOC.NE.INTR)GOTO 30
      CALL IGGRID(X(2),Y(2))
      CALL IEALPT(X,Y)
      DX=X(2)-X(1)
      DY=Y(2)-Y(1)
*
*              Delete on the screen
*
      CALL IZSAV
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      LNDECO=LHNT
      LIDECO=LDI
      LFDECO=LDF
      LCDECO=LDC
      CALL IZDATT(0,NPRIM)
      CALL ISFACI(0)
      CALL ISTXCI(0)
      CALL ISPLCI(0)
      CALL ISPMCI(0)
      CALL ISFAIS(1)
      CALL ISLN(1)
      CALL IZDNB(LHNT,LDI,LDF,LDC,NPRIM,-1)
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
*              Change in the data structure
*
      IF(IQ(LHNT+NPRIM).GT.0)THEN
         IF(IZPUSH(3,2,0,'IEMOPR').NE.0)GOTO 50
         CALL IZSTCC(INOPCO,INTPTR)
         IQ(LHI+INTPTR)=3000000+IFLPTR
         IQ(LHI+INTPTR+1)=IZGADR(LHNT,NPRIM)
         IQ(LHI+INTPTR+2)=NPRIM
         CALL IZINCI(3)
         Q(LHF+IFLPTR)=DX
         Q(LHF+IFLPTR+1)=DY
         CALL IZINCF(2)
         IQ(LHNT+NPRIM)=-(1000000*IZGCOD(LHNT,NPRIM)+ITBPTR-1)
      ELSE
         IADRI2=ABS(IZGADR(LHNT,NPRIM))
         IADRI3=IADRI2
   40    IF(IQ(LHNT+IADRI2).LT.0)THEN
            IADRI2=ABS(IZGADR(LHNT,IADRI2))
            IADRI3=IADRI2
            GOTO 40
         ENDIF
         IADRI2=IZGADR(LHNT,IADRI2)
         IF(IZPUSH(3,2,0,'IEMOPR').NE.0)GOTO 50
         CALL IZSTCC(INOPCO,INTPTR)
         IQ(LHI+INTPTR)=3000000+IFLPTR
         IQ(LHI+INTPTR+1)=IADRI2
         IQ(LHI+INTPTR+2)=IADRI3
         CALL IZINCI(3)
         Q(LHF+IFLPTR)=DX
         Q(LHF+IFLPTR+1)=DY
         CALL IZINCF(2)
         IQ(LHNT+IADRI3)=-(1000000*IZGCOD(LHNT,IADRI3)+ITBPTR-1)
      ENDIF
*
*              Change on the screen
*
      CALL IZSET
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      LNDECO=LHNT
      LIDECO=LDI
      LFDECO=LDF
      LCDECO=LDC
      CALL IZDATT(0,NPRIM)
      CALL IZDNB(LHNT,LDI,LDF,LDC,NPRIM,1)
*
   50 ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      GOTO 10
*
      END
