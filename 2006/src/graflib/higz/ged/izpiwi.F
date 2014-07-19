*
* $Id: izpiwi.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: izpiwi.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.10/00 16/02/90  10.52.35  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZPIWI(PNAME)
*.===========>
*.
*.   This routine wipe the picture PNAME after editing.
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hicode.inc"
      CHARACTER*(*) PNAME
*.______________________________________
*
      CALL IZCOPY(PNAME,'PICWIP','C')
*
*              Delete the trailing attributes codes at the end
*            of each NT bank. This codes are not necessary because
*            there is no primitives after.
*
      CALL IZPICT('PICWIP','C')
      N=IQ(LPICT+8)
      LN=LQ(LPICT-1)
      DO 30 I=1,N
         CALL ISELNT(IQ(LN+1))
   10    IF(ITBPTR.EQ.10)GOTO 20
         IKCODE=IZGCOD(LHNT,ITBPTR-1)
         IF(IKCODE.GE.49)THEN
            IQ(LHNT+ITBPTR-1)=0
            ITBPTR=ITBPTR-1
            IQ(LHNT+4)=ITBPTR
            CALL IZCFA(IKCODE,1)
            GOTO 10
         ELSEIF(IKCODE.EQ.IUNDCO)THEN
            IQ(LHNT+ITBPTR-1)=0
            ITBPTR=ITBPTR-1
            IQ(LHNT+4)=ITBPTR
            GOTO 10
         ENDIF
   20    LN=LQ(LHNT)
         IF(LN.EQ.0)GOTO 40
   30 CONTINUE
*
   40 CALL IZPICT(PNAME,'S')
      CALL IZCOPY('PICWIP',PNAME,'R')
      END
