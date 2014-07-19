*
* $Id: izdipi.F,v 1.2 1998/12/01 15:48:40 couet Exp $
*
* $Log: izdipi.F,v $
* Revision 1.2  1998/12/01 15:48:40  couet
* - Clean up: commented lines of code removed
*
* Revision 1.1.1.1  1996/02/14 13:11:09  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.20/07 12/01/94  14.04.38  by  O.Couet
*-- Author :
      SUBROUTINE IZDIPI(LP)
*.===========>
*.
*.   This routine displays the picture which is at the adress LP in the
*. ZEBRA data structure. For more details about the structure of the
*. HIGZ pictures see the comments in the routine IZPICT .
*.
*. _Input parameters:
*.
*.  INTEGER LP : Picture adress in the ZEBRA data structure .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
#include "higz/himeta.inc"
#include "higz/hiconv.inc"
      EQUIVALENCE (LPCOP,LN)
      LOGICAL GFSAV,ZFSAV,LOSIZE
*.______________________________________
*
      IF(LP.LT.0)RETURN
*
      LPICD=LP
      LDNT0=LQ(LPICD-1)
      IHP=IZGHNP(LPICD)-1
      LDNT=IZGNGT(LPICD,IHP)
      LDI=LQ(LPICD-2)
      LDF=LQ(LPICD-3)
      LDC=LQ(LPICD-4)
*
      GFSAV=GFLAG
      ZFSAV=ZFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      IF((.NOT.TEKACT).AND.(METACT).AND.(PFLAG).OR.MFLAG)THEN
         GFLAG=.FALSE.
      ELSE
         GFLAG=.TRUE.
      ENDIF
*
*              Compute in LN1 the bank adress of the NT 1
*
      LN=LDNT0
      LN1=0
      DO 10 I=1,IQ(LPICD+8)
         IF(LN.EQ.0)GOTO 20
         IF(IQ(LN+1).EQ.1)THEN
            LN1=LN
            GOTO 20
         ENDIF
         LN=LQ(LN)
   10 CONTINUE
*
   20 IF(JBIT(IQ(LPICD),1).NE.0)CALL ICLRWK(0,0)
*
      IF(LN1.NE.0)THEN
         CALL IGRNG(Q(LDF+IQ(LN1+2)+1)
     +,             Q(LDF+IQ(LN1+2)+3))
         IWVA=IQ(LN1+2)
         X1MIN=Q(LDF+IWVA+4)
         X1MAX=Q(LDF+IWVA+5)
         Y1MIN=Q(LDF+IWVA+6)
         Y1MAX=Q(LDF+IWVA+7)
         X2MIN=RQUEST(12)
         X2MAX=RQUEST(14)+RQUEST(12)
         Y2MIN=RQUEST(13)
         Y2MAX=RQUEST(15)+RQUEST(13)
         LOSIZE=.TRUE.
      ELSE
         LOSIZE=.FALSE.
      ENDIF
*
*              Display the NT banks according to there priorities
*
      NGT=0
      LN=IZGNGT(LPICD,NGT)
      DO 40 I=1,IQ(LPICD+8)
         IF(LN.EQ.0)GOTO 50
*
* Set the normalisation transformation
*
         IWVA=IQ(LN+2)
         INTN=IQ(LN+1)
*
         IF(INTN.EQ.0)GOTO 30
         CALL ISWN(INTN
     +,            Q(LDF+IWVA)
     +,            Q(LDF+IWVA+1)
     +,            Q(LDF+IWVA+2)
     +,            Q(LDF+IWVA+3))
         IF(LOSIZE)CALL IGCONV(2,Q(LDF+IWVA+4),Q(LDF+IWVA+6))
         CALL ISVP(INTN
     +,            Q(LDF+IWVA+4)
     +,            Q(LDF+IWVA+5)
     +,            Q(LDF+IWVA+6)
     +,            Q(LDF+IWVA+7))
   30    CALL ISELNT(INTN)
*
* Decode the NT bank linked to LN
*
         CALL IZDNB(LN,LDI,LDF,LDC,0,0)
*
* Give the link of the next highest priority NT bank
*
         NGT=IQ(LN+3)
         LN=IZGNGT(LPICD,NGT)
   40 CONTINUE
*
   50 ZFLAG=ZFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=GFSAV
      END
#endif
