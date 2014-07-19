*
* $Id: izscpi.F,v 1.2 1998/12/01 15:48:42 couet Exp $
*
* $Log: izscpi.F,v $
* Revision 1.2  1998/12/01 15:48:42  couet
* - Clean up: commented lines of code removed
*
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.14/09 02/06/92  10.08.58  by  O.Couet
*-- Author :
      SUBROUTINE IZSCPI(LP)
*.===========>
*.
*.   Sets the picture LP as current picture .
*.
*. _Input parameters:
*.
*.  INTEGER LP : Picture pointer .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatnb.inc"
#include "higz/hiflat.inc"
#include "higz/hiaadr.inc"
      DIMENSION IRFLAT(NBRATT),IIFLAT(NBIATT)
      EQUIVALENCE (IRFLAT(1),KMKSC),(IIFLAT(1),KPLCI)
*.______________________________________
*
      LPICT=LP
      IF(LPICT.LE.0)RETURN
*
      INBNT=IQ(LPICT+8)
      LHNT0=LQ(LPICT-1)
      LHI=LQ(LPICT-2)
      LHF=LQ(LPICT-3)
      LHC=LQ(LPICT-4)
      LN=LHNT0
      LHNT=LHNT0
      LHNTLA=LHNT0
*
*              Sets the normalisation transformation with
*            the highest priority as the current transformation.
*
      DO 10 I=1,INBNT
         LN=LQ(LN)
         IF(LN.EQ.0)GOTO 20
         LHNTLA=LN
         IF(IQ(LN+3).GT.IQ(LHNT+3))LHNT=LN
   10 CONTINUE
*
   20 CONTINUE
      ITBPTR=IQ(LHNT+4)
      INBLEV=IQ(LHNT+5)
      IRNSIZ=IQ(LHNT-1)
*
*              Set the Flags Attributes
*              Initialise the attributes adresses
*
      DO 30 I=1,NBIATT
         IIFLAT(I)=JBIT(IQ(LHNT+6),I)
         IATADR(30+I)=-1
   30 CONTINUE
      DO 40 I=1,NBRATT
         IRFLAT(I)=JBIT(IQ(LHNT+7),I)
         IATADR(I)=-1
   40 CONTINUE
*
*              Set the adress of the default attributes
*
      IIAPTR=IZGADR(LHNT,8)
      IRAPTR=IZGADR(LHNT,9)
*
*              Set pointers and bank size
*
      INTPTR=IQ(LPICT+2)
      IFLPTR=IQ(LPICT+3)
      ISTPTR=IQ(LPICT+4)
      IRISIZ=IQ(LPICT+5)
      IRFSIZ=IQ(LPICT+6)
      IRSSIZ=IQ(LPICT+7)
*
      END
#endif
