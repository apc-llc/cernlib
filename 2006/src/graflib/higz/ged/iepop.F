*
* $Id: iepop.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iepop.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.47.26  by  O.Couet
*-- Author :    O.Couet   16/01/89
      SUBROUTINE IEPOP(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
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
      CALL IZSAV
      LNDECO=LHNT
      LIDECO=LDI
      LFDECO=LDF
      LCDECO=LDC
      CALL IZDATT(0,NPRIM)
      CALL IZDNB(LHNT,LHI,LHF,LHC,NPRIM,1)
      IQ(LHNT+NPRIM)=1000000
*
   30 CALL IZSET
      GOTO 10
*
      END

