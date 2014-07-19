*
* $Id: imwrit.F,v 1.2 1998/01/30 15:22:42 couet Exp $
*
* $Log: imwrit.F,v $
* Revision 1.2  1998/01/30 15:22:42  couet
* - APOLLO version removed
*
* Revision 1.1.1.1  1996/02/14 13:11:02  mclareni
* Higz
*
*
#if defined(CERNLIB_MAIL)
#include "higz/pilot.h"
*CMZ :  1.14/00 20/02/92  13.58.53  by  O.Couet
*-- Author :    O.Couet   13/02/89
      SUBROUTINE IMWRIT(IMC)
*.===========>
*.
*.   This routine write CHMAIL on the std output.
*.
*. _Input parameters:
*.
*.  INTEGER IMC : Control, IMC=1 <==> First record
*.                         IMC=2 <==> Middle record
*.                         IMC=3 <==> Last record
*.                         IMC=4 <==> 1+2+3
*.                         IMC=5 <==> Only CHCTRL
*.
*..==========> (O.Couet)
#include "higz/himail.inc"
*.______________________________________
*
      IF(IMC.EQ.5)THEN
         WRITE (6,10400) CHCTRL
         RETURN
      ENDIF
*
      IF(IGASCI(CHMAIL(1:1)).LT.32)CHMAIL=' '
      ILEN=LENOCC(CHMAIL)
      IF(ILEN.EQ.0)ILEN=2
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_NEWLIB))
      ICR=IOSCLR()
#endif
*
      GOTO(10,20,30,40)IMC
*
   10 WRITE (6,10000) CHCTRL,CHEOR,CHMAIL(1:ILEN),CHEOR
      GOTO 50
*
   20 WRITE (6,10100) CHEOR,CHMAIL(1:ILEN),CHEOR
      GOTO 50
*
   30 WRITE (6,10200) CHEOR,CHMAIL(1:ILEN),CHEOR,CHCTRL
      GOTO 50
*
   40 WRITE (6,10300) CHCTRL,CHEOR,CHMAIL(1:ILEN),CHEOR,CHCTRL
*
   50 CHMAIL=' '
      RETURN
*
#if defined(CERNLIB_UNIX)
10000 FORMAT(A2,A1,'%',A,A1)
10100 FORMAT(A1,A,A1)
10200 FORMAT(A1,A,A1,A2)
10300 FORMAT(A2,A1,'%',A,A1,A2)
10400 FORMAT(A2)
#endif
#if (!defined(CERNLIB_UNIX))
10000 FORMAT(1X,A2,A1,'%',A,A1)
10100 FORMAT(1X,A1,A,A1)
10200 FORMAT(1X,A1,A,A1,A2)
10300 FORMAT(1X,A2,A1,'%',A,A1,A2)
10400 FORMAT(1X,A2)
#endif
      END
#endif
