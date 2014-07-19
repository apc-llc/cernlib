*
* $Id: izpush.F,v 1.2 1998/12/01 15:48:41 couet Exp $
*
* $Log: izpush.F,v $
* Revision 1.2  1998/12/01 15:48:41  couet
* - Clean up: commented lines of code removed
*
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.14/09 01/06/92  16.36.30  by  O.Couet
*-- Author :
      FUNCTION IZPUSH(IIDS,IFDS,ISDS,CHROUT)
*.===========>
*.
*.   This function enlarge the (NT),(I),(F) and (C) bank when there is
*. not enough place to put data into one of this bank .
*.   If IZPUSH .EQ. 0 the pushing is OK if not , there is not enough
*. space in memory .
*.
*. _Input parameters:
*.
*.  INTEGER IIDS : Integer Data Size to put into (I) .
*.  INTEGER IFDS : Floating point Data Size to put into (F) .
*.  INTEGER ISDS : Character Data Size to put into (C) .
*.  CHARACTER CHROUT : Calling routine name (in case of error) .
*.
*. N.B. For the current NT bank the number of new data is always 1.
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
      CHARACTER*(*) CHROUT
*.______________________________________
*
      IZPUSH=0
      IF(.NOT.ZFLAG)RETURN
      IF(LPICT.LE.0)THEN
         CALL IGERR('No current picture',CHROUT)
         RETURN
      ENDIF
*
*              Enlarge the current NT bank
*
      IF(ITBPTR+1.GT.IRNSIZ)THEN
         INPUSH=2+INSIZ-IRNSIZ+ITBPTR-1
         INPUSH=MAX(INPUSH,IQ(LHNT-1)/4)
         NWNEED=IQ(LHNT-1)+IQ(LHNT-2)+INPUSH+25
         CALL MZNEED(IXHIGZ,NWNEED,'G')
         IF(IQUEST(11).LT.0)GOTO 10
         CALL MZPUSH(IXHIGZ,LHNT,0,INPUSH,' ')
         IRNSIZ=IRNSIZ+INPUSH
      ENDIF
*
*              Enlarge the INTEGER bank
*
      IF(INTPTR+IIDS.GT.IRISIZ+1)THEN
         IIPUSH=IIDS+IISIZ-IRISIZ+INTPTR-1
         IIPUSH=MAX(IIPUSH,IQ(LHI-1)/4)
         NWNEED=IQ(LHI-1)+IQ(LHI-2)+IIPUSH+25
         CALL MZNEED(IXHIGZ,NWNEED,'G')
         IF(IQUEST(11).LT.0)GOTO 10
         CALL MZPUSH(IXHIGZ,LHI,0,IIPUSH,' ')
         IRISIZ=IRISIZ+IIPUSH
         IQ(LPICT+5)=IRISIZ
      ENDIF
*
*              Enlarge the REAL bank
*
      IF(IFLPTR+IFDS.GT.IRFSIZ+1)THEN
         IFPUSH=IFDS+IFSIZ-IRFSIZ+IFLPTR-1
         IFPUSH=MAX(IFPUSH,IQ(LHF-1)/4)
         NWNEED=IQ(LHF-1)+IQ(LHF-2)+IFPUSH+25
         CALL MZNEED(IXHIGZ,NWNEED,'G')
         IF(IQUEST(11).LT.0)GOTO 10
         CALL MZPUSH(IXHIGZ,LHF,0,IFPUSH,' ')
         IRFSIZ=IRFSIZ+IFPUSH
         IQ(LPICT+6)=IRFSIZ
      ENDIF
*
*              Enlarge the CHARACTER bank
*
      IF(ISTPTR+ISDS.GT.IRSSIZ+1)THEN
         ISPUSH=ISDS+ISSIZ-IRSSIZ+ISTPTR-1
         ISPUSH=MAX(ISPUSH,IQ(LHC-1)/4)
         NWNEED=IQ(LHC-1)+IQ(LHC-2)+ISPUSH+25
         CALL MZNEED(IXHIGZ,NWNEED,'G')
         IF(IQUEST(11).LT.0)GOTO 10
         CALL MZPUSH(IXHIGZ,LHC,0,ISPUSH,' ')
         IRSSIZ=IRSSIZ+ISPUSH
         IQ(LPICT+7)=IRSSIZ
      ENDIF
      RETURN
*
*             Error. Not enough space left
*
   10 IZPUSH=1
      CALL IGERR('Not enough space in memory',CHROUT)
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
#endif
