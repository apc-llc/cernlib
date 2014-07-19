*
* $Id: izscan.F,v 1.1.1.1 1996/02/14 13:11:13 mclareni Exp $
*
* $Log: izscan.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.11/00 01/10/90  12.08.29  by  O.Couet
*-- Author :
      SUBROUTINE IZSCAN(LN,IFCODE,IADRS,ISEN,IPOS)
*.===========>
*.
*.   This routine scans the NT bank at the adress LN and returns the
*. position of the graphic function number IFCODE in this bank .
*.
*. _Input parameters:
*.
*.  INTEGER LN : Link of th NT bank to be scanned .
*.  INTEGER IFCODE : Graphic function code .
*.  INTEGER IADRS : Starting adress . if IADRS < 0 IFCODE is not a function code
*.                  but an integer word .
*.  INTEGER ISEN : Sense of scanning . If ISEN < 0 the scanning is reverve .
*.
*. _Output parameters:
*.
*.  INTEGER IPOS : Position, in the NT bank, of the function IFCODE .
*.                 If IPOS equal 0: The function has not be found .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      IPOS=0
      IF(IADRS.LT.0)GOTO 30
*
      IF(ISEN.GE.0)THEN
         DO 10 I=IADRS,IQ(LN+4)-1
            IF(IZGCOD(LN,I).NE.IFCODE)GOTO 10
            IPOS=I
            RETURN
  10     CONTINUE
      ELSE
         DO 20 I=IADRS,10,-1
            IF(IZGCOD(LN,I).NE.IFCODE)GOTO 20
            IPOS=I
            RETURN
  20     CONTINUE
      ENDIF
      RETURN
*
  30  IF(ISEN.GE.0)THEN
         DO 40 I=ABS(IADRS),IQ(LN+4)-1
            IF(IQ(LN+I).NE.IFCODE)GOTO 40
            IPOS=I
            RETURN
  40     CONTINUE
      ELSE
         DO 50 I=ABS(IADRS),10,-1
            IF(IQ(LN+I).NE.IFCODE)GOTO 50
            IPOS=I
            RETURN
  50     CONTINUE
      ENDIF
*
      END
#endif
