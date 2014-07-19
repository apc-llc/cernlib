*
* $Id: izundo.F,v 1.1.1.1 1996/02/14 13:10:25 mclareni Exp $
*
* $Log: izundo.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:25  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 11/01/89  12.01.49  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZUNDO
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatt.inc"
#include "higz/hicode.inc"
#include "higz/hiflag.inc"
*.______________________________________
*
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
*
   10 IF(ITBPTR.EQ.10)GOTO 30
      IKCODE=IZGCOD(LHNT,ITBPTR-1)
      IF(IKCODE.EQ.IUNDCO)GOTO 30
*
      IF(IKCODE.EQ.1)THEN
         IADRI=IZGADR(LHNT,ITBPTR-1)
         IADRNT=IQ(LHI+IADRI+2)
         IQ(LHNT+IADRNT)=ABS(1000000*IZGCOD(LHNT,IADRNT))
     +                       +IQ(LHI+IADRI+1)
         IQ(LHNT+ITBPTR-1)=0
         ITBPTR=ITBPTR-1
         IQ(LHNT+4)=ITBPTR
         GOTO 20
      ENDIF
*
      IF(IKCODE.GE.50)THEN
         IQ(LHNT+ITBPTR-1)=0
         ITBPTR=ITBPTR-1
         IQ(LHNT+4)=ITBPTR
         CALL IZCFA(IKCODE,1)
         GOTO 10
      ENDIF
*
      IF(IKCODE.LT.50)THEN
         IQ(LHNT+ITBPTR-1)=0
         ITBPTR=ITBPTR-1
         IQ(LHNT+4)=ITBPTR
      ENDIF
*
   20 CALL IGCLWN(INTR)
      CALL IZDNB(LHNT,LDI,LDF,LDC,0,0)
*
   30 ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      END

