*
* $Id: w16mvstv.F,v 1.1.1.1 1996/03/08 15:21:48 mclareni Exp $
*
* $Log: w16mvstv.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:48  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_STF77VX)
      SUBROUTINE W16MOV(SOURCE,N1,TARGET,N2,N3)
      INTEGER SOURCE(*),TARGET(*)
C
C  F77 VAX version to do W16MOV with NO external calls.
C
C.  MOVES 16 BIT WORDS IN PACKED FORM.
C.
C. SOURCE     SOURCE ARRAY
C. N1         STARTING 16 BIT WORD IN SOURCE
C. TARGET     TARGET ARRAY
C. N2         STARTING 16 BIT WORD IN TARGET
C. N3         NO. OF 16 BIT WORDS TO MOVE
C
C  16-bit word number within Computer word increases
C  as one goes from Most Significant Bit (MSB) to LSB.
C  i.e.
C     63       48 47       32 31      16 15       0
C    -----------------------------------------------
C    |     1     |     2     |     3    |     4    |
C    -----------------------------------------------
C
#include "epio/wordsize.inc"
C
      IF(N3 .LE. 0) GO TO 999
C
C
C                  SOURCE word
      IWSRC =  (N1-1)/N16PW
      I16SRC = N1-IWSRC*N16PW
      IWSRC  = IWSRC+1
      NBITR  = (I16SRC-1)*16
C
C                 And same for TARGET:
      IWTGT  = (N2-1)/N16PW
      I16TGT = N2-IWTGT*N16PW
      IWTGT  = IWTGT+1
      NBITL  = (I16TGT-1)*16
C
      DO 10 IW16=1,N3
C
C
C                  Extract 16-bit word from source
      JWD16 = ISHFT(SOURCE(IWSRC),-NBITR)
C
C                 Shift to correct position for insertion into TARGET
      JWD16 = ISHFT(JWD16,NBITL)
C
C                 Clear TARGET position
      TARGET(IWTGT) = IAND(TARGET(IWTGT),NOT(ISHFT(65535,NBITL)))
C
C                 Insert:
      TARGET(IWTGT) = IOR(TARGET(IWTGT),JWD16)
C
C                 Update indices
      IF(NBITR.EQ.NBITPW-16) THEN
         NBITR = 0
         IWSRC = IWSRC + 1
      ELSE
         NBITR = NBITR + 16
      ENDIF
C
      IF(NBITL.EQ.NBITPW-16) THEN
         NBITL = 0
         IWTGT = IWTGT + 1
      ELSE
         NBITL = NBITL + 16
      ENDIF
C
 10   CONTINUE
C
 999  END
#endif
