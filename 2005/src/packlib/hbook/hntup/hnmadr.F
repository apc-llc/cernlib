*
* $Id: hnmadr.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnmadr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 07/09/93  19.26.34  by  Fons Rademakers
*-- Author :    Fons Rademakers   19/12/91
      SUBROUTINE HNMADR(VAR1, IADD, ISCHAR)
*.==========>
*.
*.           Set the address for the variable VAR in the LNAME bank starting
*.           at address IADD. If VAR='*' set the addresses of all variables
*.           in the LNAME bank. If ISCHAR is TRUE set the addresses for
*.           character variables.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcnt.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) VAR1
      CHARACTER*32  NAME, VAR
      INTEGER       IADD
      LOGICAL       ISCHAR, ALL, LDUM
*
      VAR  = VAR1
      CALL CLTOU(VAR)
      LVAR = LENOCC(VAR)
      ALL  = .FALSE.
      IF (VAR(1:1).EQ.'*' .AND. LVAR.EQ.1) ALL = .TRUE.
*
      IOFF = 0
      NDIM = IQ(LBLOK+ZNDIM)
*
      DO 30 I = 1, NDIM
         CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
         LL = IQ(LNAME+IOFF+ZLNAME)
         LV = IQ(LNAME+IOFF+ZNAME)
         NAME = ' '
         CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
         CALL CLTOU(NAME)
*
         IF (.NOT.ALL .AND. VAR(1:LVAR).NE.NAME(1:LL)) GOTO 20
         IF (ISCHAR .AND. ITYPE.NE.5)                  GOTO 20
         IF (.NOT.ISCHAR .AND. ITYPE.EQ.5)             GOTO 20
*
         IELEM = 1
         DO 10 J = 1, NSUB
            LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
            IF (LP .LT. 0) THEN
               IE = -LP
            ELSE
               LL = IQ(LNAME+LP-1+ZRANGE)
               IE = IQ(LINT+LL+1)
            ENDIF
            IELEM = IELEM*IE
   10    CONTINUE
*
         IADDW = ISHFT(IADD, -2)
         IBYOF = IAND(IADD, IBYPW-1)
*
         IF (IBYOF .NE. 0) GOTO 40
*
         IQ(LNAME+IOFF+ZNADDR) = IADDW - LOCF(IQ(1))
         IADD = IADD + IELEM*ISIZE
*
   20    IOFF = IOFF + ZNADDR
   30 CONTINUE
*
      RETURN
*
   40 PRINT *, 'Variable ', NAME(1:LENOCC(NAME))
      CALL HBUG('Address not word aligned','HBNAME',ID)
      RETURN
*
      END
