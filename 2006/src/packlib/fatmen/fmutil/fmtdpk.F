*
* $Id: fmtdpk.F,v 1.1.1.1 1996/03/07 15:18:17 mclareni Exp $
*
* $Log: fmtdpk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:17  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMTDPK(DATM)
C        LAST CORRECTION DATE IS JULY 14,1989. A. SHEVEL
C    THIS SUBROUTINE WAS PREPARED TO GET DATE                   !
C    IN PACKED FORM 'YMD'                                       !
      INTEGER     IT,ID,IY,IM,K,FMTIME
      CHARACTER*3 DATM
      CHARACTER*1 YY,MM,DD,T(31)
C
      ICODE=FMTIME(ID,IT)
C
C              TRANSFORM THE DATE INTO 3-BYTE
C                   IN THE FORM 'YMD'
C
      IY = ID / 10000
      IM = ID / 100 - IY * 100
      ID = ID - (IY * 10000 + IM * 100)
C
      DO 10 I = 1,10
         K = 80 + (I-1) * 10
        IF ( (IY-K) .LE. 9 ) THEN
                  IY = IY - K
                  GOTO 1
        ENDIF
10    CONTINUE
  1   WRITE (YY,'(I1)') IY
C
      IF (IM .GT. 12) IM = 12
      IF (IM .LT.  1) IM =  1
C
      IF (ID .GT. 31) ID = 31
      IF (ID .LT.  1) ID =  1
C
      T(1) = '1'
      T(2) = '2'
      T(3) = '3'
      T(4) = '4'
      T(5) = '5'
      T(6) = '6'
      T(7) = '7'
      T(8) = '8'
      T(9) = '9'
      T(10) = 'A'
      T(11) = 'B'
      T(12) = 'C'
      T(13) = 'D'
      T(14) = 'E'
      T(15) = 'F'
      T(16) = 'G'
      T(17) = 'H'
      T(18) = 'I'
      T(19) = 'J'
      T(20) = 'K'
      T(21) = 'L'
      T(22) = 'M'
      T(23) = 'N'
      T(24) = 'O'
      T(25) = 'P'
      T(26) = 'Q'
      T(27) = 'R'
      T(28) = 'S'
      T(29) = 'T'
      T(30) = 'U'
      T(31) = 'V'
C
      DD = T(ID)
      MM = T(IM)
      DATM = YY // MM // DD
      FMTDPK=0
      RETURN
      END
