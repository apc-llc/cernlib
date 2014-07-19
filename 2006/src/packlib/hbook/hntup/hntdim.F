*
* $Id: hntdim.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hntdim.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 31/03/93  14.36.19  by  Fons Rademakers
*-- Author :    Fons Rademakers   31/03/93
      SUBROUTINE HNTDIM(TVAR, NVDIM, IVDIM, MXVDIM)
*.==========>
*.
*.           Returns the number of dimensions (NVDIM) and the
*.           dimensions (IVDIM) of the variable TVAR.
*.           MXVDIM gives the size of the array IVDIM (the maximum
*.           number of dimensions an Ntuple variable may have is 7).
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) TVAR
      CHARACTER*32  VAR, NAME
      INTEGER       IVDIM(*)
*
*-- save block address (not in link area but in this routine no
*-- relocation should happen so it will be ok)
*
      LOLDBL = LBLOK
*
      VAR    = TVAR
      CALL CLTOU(VAR)
      LVAR   = LENOCC(VAR)
      LBLOK  = LQ(LCID-1)
*
      CALL VZERO(IVDIM, MXVDIM)
      NVDIM = 0
*
*-- loop over all blocks
*
   10 LNAME = LQ(LBLOK-1)
*
      NDIM = IQ(LBLOK+ZNDIM)
      IOFF = 0
*
      DO 20 I = 1, NDIM
         LL = IQ(LNAME+IOFF+ZLNAME)
         LV = IQ(LNAME+IOFF+ZNAME)
         NAME = ' '
         CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
         CALL CLTOU(NAME)
         IF (VAR(1:LVAR) .EQ. NAME(1:LL)) THEN
*
            NSUB  = JBYT(IQ(LNAME+IOFF+ZDESC), 18, 3)
            DO 25 J = 1, NSUB
               IF (J .GT. MXVDIM) GOTO 25
               LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
               IF (LP .LT. 0) THEN
                  IVDIM(J) = -LP
               ELSE
                  LL = IQ(LNAME+LP-1+ZRANGE)
                  IVDIM(J) = IQ(LINT+LL+1)
               ENDIF
   25       CONTINUE
*
            NVDIM = NSUB
            GOTO 30
         ENDIF
         IOFF = IOFF + ZNADDR
   20 CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 10
*
   30 LBLOK = LOLDBL
      LNAME = LQ(LBLOK-1)
*
      END
