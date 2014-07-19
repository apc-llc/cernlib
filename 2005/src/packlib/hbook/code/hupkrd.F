*
* $Id: hupkrd.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: hupkrd.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.54  by  Rene Brun
*-- Author :
      SUBROUTINE HUPKRD(LUN,NEXT,MEMSWA)
*.==========>
*.       Unpack machine independent 32-bit block .Read input
*.       buffer from file LUN
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      COMMON/HMIBUF/MISCUR,NBUFMI,LUNTYP,NEWBUF,LCBUFF,LUBUFF,MIBEND
      COMMON/HFORM/I5BUFF(128)
*.___________________________________________
      IF(NBUFMI.EQ.0) GO TO 999
      L = MEMSWA+LUBUFF-1
      LBLOCK = 63
      DO 40 I = MEMSWA,L,64
         LI5BUF = L-I
         IF(LI5BUF.LT.63) LBLOCK = LI5BUF
         READ(LUN,1000)I5BUFF
         K = I+LBLOCK
         I5NEXT = 1
         DO 30 J = I,K
            IQ(J) = I5BUFF(I5NEXT+1)
            CALL SBYT(I5BUFF(I5NEXT),IQ(J),17,16)
            I5NEXT = I5NEXT+2
  30     CONTINUE
  40  CONTINUE
      NBUFMI = IQ(MEMSWA)
      MIBEND = MEMSWA+NBUFMI-1
      MISCUR = MEMSWA+4
      NEXT = MISCUR+1
      IF(NEWBUF.EQ.0) NEXT = MISCUR-1
      NEWBUF = 1
999   RETURN
1000  FORMAT(16I5)
      END
