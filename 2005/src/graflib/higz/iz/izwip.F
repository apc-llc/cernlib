*
* $Id: izwip.F,v 1.2 1998/12/01 15:48:42 couet Exp $
*
* $Log: izwip.F,v $
* Revision 1.2  1998/12/01 15:48:42  couet
* - Clean up: commented lines of code removed
*
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.14/09 01/06/92  16.36.30  by  O.Couet
*-- Author :
      SUBROUTINE IZWIP(LP)
*.===========>
*.
*.   This routine supress the empty words of the picture LP .
*.
*. _Input parameters:
*.
*.  INTEGER LP : Picture link .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      IF(LP.LE.0)RETURN
      LPSAV=LPICT
      CALL IZSCPI(LP)
*
*              Cleaning of the NT banks
*
      LN=LHNT0
      DO 10 I=1,INBNT
         IF(LN.EQ.0)GOTO 30
         INPUSH=IQ(LN+4)-IQ(LN-1)
         IF(INPUSH.EQ.0)GOTO 20
         CALL MZPUSH(IXHIGZ,LN,0,INPUSH,' ')
         IF(LN.EQ.LHNT)IRNSIZ=IQ(LN-1)
 20      LN=LQ(LN)
 10   CONTINUE
 30   CONTINUE
*
*              Cleaning of the INTEGER bank
*
      IIPUSH=INTPTR-IRISIZ
      IF(IIPUSH.NE.0)THEN
         CALL MZPUSH(IXHIGZ,LHI,0,IIPUSH,' ')
         IRISIZ=IRISIZ+IIPUSH
         IQ(LPICT+5)=IRISIZ
      ENDIF
*
*              Cleaning of the REAL bank
*
      IFPUSH=IFLPTR-IRFSIZ
      IF(IFPUSH.NE.0)THEN
         CALL MZPUSH(IXHIGZ,LHF,0,IFPUSH,' ')
         IRFSIZ=IRFSIZ+IFPUSH
         IQ(LPICT+6)=IRFSIZ
      ENDIF
*
*              Cleaning of the CHARACTER bank
*
      ISPUSH=ISTPTR-IRSSIZ
      IF(ISPUSH.NE.0)THEN
         CALL MZPUSH(IXHIGZ,LHC,0,ISPUSH,' ')
         IRSSIZ=IRSSIZ+ISPUSH
         IQ(LPICT+7)=IRSSIZ
      ENDIF
*
      IF(LPSAV.GT.0)THEN
         CALL IZSCPI(LPSAV)
      ELSE
         LPICT=LPSAV
      ENDIF
*
      END
#endif
