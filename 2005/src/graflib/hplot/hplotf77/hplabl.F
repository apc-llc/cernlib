*
* $Id: hplabl.F,v 1.1.1.1 1996/01/19 10:49:57 mclareni Exp $
*
* $Log: hplabl.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:57  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/16 25/08/94  15.51.16  by  O.Couet
*-- Author :
      SUBROUTINE HPLABL(NUM,NB,CHLAB)
*.===========>
*.
*.   This routine stores the label list number NUM. This routine limits
*. the lenght of the alphanumeric labels at 32 characters.
*.
*. _Input parameters:
*.
*.  INTEGER NUM : List number .
*.  INTEGER NB : Number of labels .
*.  CHARACTER CHLAB(NB) : List contents.
*.
*..==========> (O.Couet)
#include "hbook/hcbook.inc"
#include "hplot/quest.inc"
#include "hplot/hplstr.inc"
      PARAMETER (LABLEN=32)
      CHARACTER*(*) CHLAB(*)
*.______________________________________
*
      IF (NB.LE.0) RETURN
      IF ((NUM.LT.1).OR.(NUM.GT.9)) THEN
         CALL HBUG('0 < NUM < 10','HPLABL',0)
         RETURN
      ENDIF
*
      LW     = LABLEN/4
      NCHP   = LW*(NB+1)
      CHTEMP = ' '
*
      CALL MZFORM('LABL','2I -H',IXIO)
      IF (LQ(LHPLOT-2).EQ.0)THEN
         CALL MZBOOK(IHDIV,LN,LHPLOT,-2,'LABL',0,0,NCHP,IXIO,0)
         IQ(LN+1) = NUM
         IQ(LN+2) = NB
         DO 10 I=1,NB
            CHTEMP = CHLAB(I)
            CALL UCTOH(CHTEMP,IQ(LN+3+(I-1)*LW),4,LABLEN)
   10    CONTINUE
      ELSE
         LDROP = LQ(LHPLOT-2)
   20    IF (IQ(LDROP+1).EQ.NUM) THEN
            CALL MZDROP(IHDIV,LDROP,' ')
            GOTO 30
         ELSE
            LDROP = LQ(LDROP)
            IF (LDROP.EQ.0) THEN
               GOTO 30
            ELSE
               GOTO 20
            ENDIF
         ENDIF
   30    LEND = LQ(LHPLOT-2)
         IF (LEND.EQ.0) THEN
            CALL MZBOOK(IHDIV,LN,LHPLOT,-2,'LABL',0,0,NCHP,IXIO,0)
         ELSE
   40       L1 = LQ(LEND)
            IF (L1.NE.0) THEN
               LEND = L1
               GOTO 40
            ELSE
               CALL MZBOOK(IHDIV,LN,LEND,0,'LABL',0,0,NCHP,IXIO,0)
            ENDIF
         ENDIF
         IQ(LN+1) = NUM
         IQ(LN+2) = NB
         DO 50 I=1,NB
            CHTEMP = CHLAB(I)
            CALL UCTOH(CHTEMP,IQ(LN+3+(I-1)*LW),4,LABLEN)
   50    CONTINUE
      ENDIF
      IQUEST(1) = 0
*
      END
