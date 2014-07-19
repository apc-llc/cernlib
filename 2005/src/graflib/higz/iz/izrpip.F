*
* $Id: izrpip.F,v 1.1.1.1 1996/02/14 13:11:13 mclareni Exp $
*
* $Log: izrpip.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.06/07 04/04/89  09.34.00  by  O.Couet
* Bugs corrected IZRPIP=LP only if NCHP=IQ(LPICT+1)
*-- Author :
      INTEGER FUNCTION IZRPIP(PNAME)
*.===========>
*.
*.   This routine Retrieves the PIcture Pointer of the picture PNAME .
*.
*. _Input parameters:
*.
*.  CHARACTER PNAME : Picture name .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
      CHARACTER*(*) PNAME
      DIMENSION INAME(5)
*.______________________________________
*
      IZRPIP=0
      IF(LHIGZ.EQ.0)RETURN
      NCHP=LENOCC(PNAME)
      IF(NCHP.GT.20)NCHP=20
      IF(NCHP.LT.1)RETURN
      CALL UCTOH(PNAME,INAME(1),4,NCHP)
      LP=LQ(LHIGZ)
      IF(LP.EQ.0)RETURN
      LCH=LQ(LP-4)
      I=0
*
   10 I=I+1
      IF(I.GT.NBPICT)RETURN
      DO 20 J=1,(NCHP+3)/4
         IF(INAME(J).NE.IQ(LCH+J))THEN
            LP=LQ(LP)
            IF(LP.EQ.0)RETURN
            LCH=LQ(LP-4)
            GOTO 10
         ENDIF
   20 CONTINUE
      IF(NCHP.EQ.IQ(LP+1))THEN
         IZRPIP=LP
         RETURN
      ELSE
         LP=LQ(LP)
         IF(LP.EQ.0)RETURN
         LCH=LQ(LP-4)
         GOTO 10
      ENDIF
*
      END
#endif
