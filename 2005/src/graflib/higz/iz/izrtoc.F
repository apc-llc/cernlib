*
* $Id: izrtoc.F,v 1.1.1.1 1996/02/14 13:11:13 mclareni Exp $
*
* $Log: izrtoc.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/10 18/01/93  13.59.56  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZRTOC(RVAL,CVAL)
*.===========>
*.
*. Convert character string CVAL into real value RVAL
*.
*..==========>
      PARAMETER (MAXDIG=16)
      CHARACTER*(MAXDIG) CHTMP1,CHTMP2
      CHARACTER*(*) CVAL
*.______________________________________
*
      IF(RVAL.EQ.0)THEN
         CVAL='0.'
         RETURN
      ENDIF
*
      IF(RVAL.GE.0.001.AND.RVAL.LE.10.)THEN
         WRITE (CHTMP1,'(F10.5)') RVAL
      ELSE
         WRITE (CHTMP1,'(G15.5)') RVAL
      ENDIF
*
*              Remove the leading blanks
*
      I1=LENOCC(CHTMP1)
   10 IF(CHTMP1(1:1).EQ.' ')THEN
         CHTMP2=CHTMP1(2:I1)
         CHTMP1=CHTMP2
         I1=I1-1
         GOTO 10
      ENDIF
*
*              Remove the trailing 0
*
      I2=INDEX(CHTMP1,'E')
      IF(I2.NE.0)THEN
   20    IF(CHTMP1(I2-1:I2-1).EQ.'0')THEN
            CHTMP2=CHTMP1(1:I2-2)//CHTMP1(I2:)
            CHTMP1=CHTMP2
            I1=I1-1
         ELSE
            GOTO 40
         ENDIF
         I2=INDEX(CHTMP1,'E')
         GOTO 20
      ELSE
   30    IF(CHTMP1(I1:I1).EQ.'0')THEN
            CHTMP2=CHTMP1(1:I1-1)
            CHTMP1=CHTMP2
            I1=I1-1
            GOTO 30
         ENDIF
      ENDIF
*
*              Add a 0 in front of the '.'
*
   40 IF(CHTMP1(1:1).EQ.'.')THEN
         CHTMP2='0'//CHTMP1
         CHTMP1=CHTMP2
         I1=I1+1
      ENDIF
*
      CVAL=CHTMP1(1:I1)
*
      END
