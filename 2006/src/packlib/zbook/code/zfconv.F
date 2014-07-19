*
* $Id: zfconv.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zfconv.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZFCONV(IW,NW,IARR,NARR,ISENSE)
*
* CONVERTS CONTENTS OF ARRAY IARR TO FLOATING OR BACK TO INTEGER
* ACCORDING TO A SPECIFIED FORMAT.
*
* author HG       7/4/84
*
* INPUT
* IW            CONVERSION FORMAT (SEE ROUTINE BKBOOK)
* NW            LENGTH OF FORMAT
* NARR          LENGTH OF IARR
* ISENSE        FLAG FOR CONVERSION DIRECTION: +1 INTERNAL TO EXTERNAL,
*               ELSE EXTERNAL TO INTERNAL.
* INPUT/OUTPUT
* IARR          ARRAY WITH CONVERTED NUMBERS
*
      DIMENSION IARR(*),IW(*)
#if defined(CERNLIB_VAX)
*--- FOR TEXT SWAPS
      INTEGER *2 INST2(2),INST2T
      EQUIVALENCE (INST4,INST2(1))
#endif
      IPNT=0
      IFNT=0
   10 IFNT=IFNT+1
      IF (IFNT.GT.NW) GOTO 70
      IF(IW(IFNT).LT.10000)  THEN
*--- REPETITION COUNT
         IF (IFNT.EQ.NW) GOTO 70
         IREP=IW(IFNT)
         IF (IREP.EQ.0) IREP=10000000
         IL1=IFNT+1
         DO 20 I=IL1,NW
            IF (IW(I).EQ.50000) GOTO 30
   20    CONTINUE
         I=NW+1
   30    CONTINUE
         IL2=I-1
         IFNT=I
      ELSE
         IREP=1
         IL1=IFNT
         IL2=IFNT
      ENDIF
   40 CONTINUE
      DO 60 I=IL1,IL2
         ICODE=IW(I)/10000
         IF (IL1.EQ.IL2.AND.IREP.GT.1000000)  THEN
*--- INFINITE COUNT ONLY ONE FORMAT
            ICNT=NARR-IPNT
         ELSE
            ICNT=MIN(NARR-IPNT,IW(I)-10000*ICODE)
         ENDIF
         IF (ICODE.GE.2.AND.ICODE.LE.3)  THEN
            IF (ISENSE.EQ.1)  THEN
               CALL CTOIBM(IARR(IPNT+1),ICNT,ICODE)
            ELSE
               CALL CFRIBM(IARR(IPNT+1),ICNT,ICODE)
            ENDIF
         ELSEIF (ICODE.EQ.4)  THEN
            IF (ISENSE.EQ.1)  THEN
               CALL ZTOIBM(IARR(IPNT+1),ICNT,ICODE)
            ELSE
               CALL ZFRIBM(IARR(IPNT+1),ICNT,ICODE)
            ENDIF
#if defined(CERNLIB_VAX)
*--- SWAP 16 BIT WORDS FOR TEXT
            DO 50 J=1,ICNT
               INST4=IARR(IPNT+J)
               INST2T=INST2(1)
               INST2(1)=INST2(2)
               INST2(2)=INST2T
   50       IARR(IPNT+J)=INST4
#endif
         ENDIF
         IPNT=IPNT+ICNT
         IF (IPNT.EQ.NARR) GOTO 999
   60 CONTINUE
      IREP=IREP-1
      IF (IREP.EQ.0) GOTO 10
      GOTO 40
   70 CONTINUE
      IF(IPNT.LT.NARR)  THEN
*--- CONVERT REST AS FLOATING
         IF (ISENSE.EQ.1)  THEN
            CALL CTOIBM(IARR(IPNT+1),NARR-IPNT,3)
         ELSE
            CALL CFRIBM(IARR(IPNT+1),NARR-IPNT,3)
         ENDIF
      ENDIF
  999 END
