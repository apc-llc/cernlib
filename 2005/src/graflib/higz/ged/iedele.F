*
* $Id: iedele.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iedele.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.47.25  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEDELE(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
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
      IF(NPRIM.LE.0)GOTO 30
      LN=IZGNTP(NT)
      IF(LN.EQ.0)GOTO 30
      CALL ISELNT(NT)
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
      CALL IZDNB(LHNT,LHI,LHF,LHC,NPRIM,-1)
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
*              Delete in the data structure
*
      IF(IQ(LHNT+NPRIM).GT.0)THEN
         IF(IZPUSH(3,0,0,'IEDELE').NE.0)GOTO 30
         CALL IZSTCC(INOPCO,INTPTR)
         IQ(LHI+INTPTR)=0
         IQ(LHI+INTPTR+1)=IZGADR(LHNT,NPRIM)
         IQ(LHI+INTPTR+2)=NPRIM
         CALL IZINCI(3)
         IQ(LHNT+NPRIM)=-(1000000*IZGCOD(LHNT,NPRIM)+ITBPTR-1)
      ELSE
         IADRI2=ABS(IZGADR(LHNT,NPRIM))
         IADRI3=IADRI2
   20    IF(IQ(LHNT+IADRI2).LT.0)THEN
            IADRI2=ABS(IZGADR(LHNT,IADRI2))
            IADRI3=IADRI2
            GOTO 20
         ENDIF
         IADRI2=IZGADR(LHNT,IADRI2)
         IF(IZPUSH(3,0,0,'IEDELE').NE.0)GOTO 30
         CALL IZSTCC(INOPCO,INTPTR)
         IQ(LHI+INTPTR)=0
         IQ(LHI+INTPTR+1)=IADRI2
         IQ(LHI+INTPTR+2)=IADRI3
         CALL IZINCI(3)
         IQ(LHNT+IADRI3)=-(1000000*IZGCOD(LHNT,IADRI3)+ITBPTR-1)
      ENDIF
*
   30 CALL IZSET
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      GOTO 10
*
      END
