*
* $Id: mveric.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: mveric.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE MVERIC (NTT,IHAVE,IMUST,NN,NCHH,NCHM,CHH,CHM)

C-    Compare the integer vectors IHAVE and IMUST of NN words,
C-                                unless NN is zero;
C-    and compare the CHARACTER strings CHH and CHM
C-                of NCHH, NCHM characters if NCHH = NCHM only

#include "kerngent/mkcde.inc"

      DIMENSION    IHAVE(99), IMUST(99)
      CHARACTER    CHH*99, CHM*99
      CHARACTER    CHPR*50

      NTX  = NTT
      NIDO = NN
      IF (NIDO.GE.201)       GO TO 91
      IF (NTEST.GE.10)       CALL PRTEST

      JIF = 0
      IF (NIDO.LE.0)         GO TO 26

      DO 24 JI=1,NIDO
      IF  (IHAVE(JI).NE.IMUST(JI))  THEN
          IF (JIF.EQ.0)  JIF = JI
        ENDIF
   24 CONTINUE

   26 IF (NCHH.EQ.NCHM)  THEN
          IF (CHH(1:NCHH).NE.CHM(1:NCHH))  GO TO 41
        ENDIF

      JCF = 0
      IF (JIF.NE.0)          GO TO 46

C----              VERIFY OK.

      IF (NFAIL.EQ.0)        GO TO 39
      CALL PRTEST
      NFAIL = 0

   39 NTEST = NTEST + 1
      MTESTV(NTEST) = NTX
      IF (NEACHP.EQ.0)       RETURN
      CALL PRTEST
      RETURN

C----              VERIFY FAILURE

   41 DO 42  JCF=1,NCHH
      IF (CHH(JCF:JCF).NE.CHM(JCF:JCF))  GO TO 43
   42 CONTINUE
   43 CONTINUE

   46 NFAILT = NFAILT + 1
      IF (NFAIL.EQ.0)        GO TO 47
      IF (NFAIL.LT.NFAIPR)   GO TO 48
      NFAIL = NFAIL + 1
      GO TO 39

   47 CALL PRTEST
   48 WRITE (ITB,9001) NTX
      NFAIL = NFAIL  + 1

C--       Dump integer vectors

      IF (JIF.EQ.0)                GO TO 61
      WRITE (ITB,9051) JIF
      J1 = JIF

      DO 52  J=JIF,NIDO
      IF (IABS(IHAVE(J)).GE.10000) GO TO 58
      IF (IABS(IMUST(J)).GE.10000) GO TO 58
   52 CONTINUE

   54 J2 = MIN (J1+9,NIDO)
      WRITE (ITB,9054) J1,(IMUST(J),J=J1,J2)
      WRITE (ITB,9055)    (IHAVE(J),J=J1,J2)
      J1 = J1 + 10
      IF (J1.LE.NIDO)  GO TO 58
      IF (J1.NE.NIDO+1)  WRITE (ITB,9013)
      GO TO 61

   58 J2 = MIN (J1+4,NIDO)
      WRITE (ITB,9011) J1,(IMUST(J),J=J1,J2)
      WRITE (ITB,9012)    (IHAVE(J),J=J1,J2)
      J1 = J1 + 5
      IF (J1.LE.NIDO)  GO TO 58
      IF (J1.NE.NIDO+1)  WRITE (ITB,9013)

C--       Dump character strings

   61 IF (JCF.EQ.0)  THEN
          WRITE (ITB,9061)
        ELSE
          WRITE (ITB,9062) JCF
        ENDIF
      ND = 0

      IF (ND.GE.NCHM)              GO TO 66

   62 NU = MIN (NCHM-ND, 50)
      CHPR(1:NU) = CHM(ND+1:ND+NU)
      WRITE (ITB,9013)

      CALL MVEUNS (CHPR(1:NU),ND)
      WRITE (ITB,9064) ND+1,CHPR(1:NU)

   66 IF (ND.GE.NCHH)              GO TO 69
   67 NU = MIN (NCHH-ND, 50)
      CHPR(1:NU) = CHH(ND+1:ND+NU)

      CALL MVEUNS (CHPR(1:NU),ND)
      WRITE (ITB,9065) CHPR(1:NU)
   69 ND = ND + 50
      IF (ND.LT.NCHM)              GO TO 62
      IF (ND.LT.NCHH)              GO TO 67
      WRITE (ITB,9013)
      RETURN

C----              Trouble

   91 CALL PRTEST
      WRITE (ITB,9091) NTX, NIDO
      RETURN

 9001 FORMAT (5H TEST,I5,' FAILED, Dump follows'/)
 9013 FORMAT (1X)
 9051 FORMAT (10X,'Dump integer vectors starting at',I4/1X)
 9054 FORMAT (I10,5H MUST,10I5)
 9055 FORMAT (10X,5H HAVE,10I5/1X)
 9061 FORMAT (10X,'Dump characters strings'/1X)
 9062 FORMAT (10X,'String mismatch at character',I5/1X)
 9064 FORMAT (I9,6H MUST ,A)
 9065 FORMAT (9X,6H HAVE ,A)
 9091 FORMAT (1X/' VERIFY VECTOR FOR TEST',I6,'  MORE THAN 200 WORDS,',
     F' NAMELY',I5)

*             FROM MACHINE PATCH
#include "verifmt.inc"

      END
