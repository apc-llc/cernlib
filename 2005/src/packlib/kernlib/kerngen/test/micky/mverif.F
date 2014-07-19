*
* $Id: mverif.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: mverif.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE MVERIF (NTT,HAVE,AMUST,NN)

#include "kerngent/mkcde.inc"

      DIMENSION  HAVE(9),AMUST(9), NTT(9), NN(9)
      DIMENSION  TEST(200)

#if defined(CERNLIB_QMVAX)
      DIMENSION    IHAVE(200), IMUST(200)
#endif


      NTUSE = NTT(1)
      NHAVE = NN(1)
      IF (NHAVE.GE.201)            GO TO 91
      ZERU = ZERLEV
      IF (NTEST.GE.10)  CALL PRTEST

#if defined(CERNLIB_GENUSE)
C----              Read/write the MUST data on binary file

      IF (IFLGU)                   11, 31, 14
   11 IF (LUNGU.EQ.0)  LUNGU= 11
      WRITE (LUNGU) NTUSE,(HAVE(J),J=1,NHAVE)
      GO TO 31

   14 IF (LUNGU.EQ.0)  LUNGU= 11
      ZERU = ZERGU
      CALL UCOPY (AMUST,SAVE,NHAVE)
      READ (LUNGU) NTN,(AMUST(J),J=1,NHAVE)
      IF (NTN.EQ.NTUSE)            GO TO 31
      WRITE (ITB,9017) NTUSE,NTN
 9017 FORMAT (1X/26H FAULTY READ-BACK FOR TEST,I4,10H,  FINDING,I4)
      STOP
#endif
#if (defined(CERNLIB_GENMUST))&&(!defined(CERNLIB_GENUSE))
C----              Generate the MUST data on "cards"

      IF (IFLGU.EQ.0)              GO TO 31
      IF (LUNGU.EQ.0)  LUNGU= ITB

      WRITE (LUNGU,9024) NTUSE
      WRITE (LUNGU,9025) (HAVE(J),J=1,NHAVE)
      RETURN

 9024 FORMAT (' DATA for test',I6)
 9025 FORMAT (6X,G20.9)
#endif

C----              Verify

   31 DO  36  JJ=1,NHAVE
      J = JJ
      DIFF = ABS (HAVE(J) - AMUST(J))
      IF (DIFF .LT. ZERU)          GO TO 36
      SUM  = ABS (HAVE(J) + AMUST(J))
      IF (SUM .LT. 2.)             GO TO 41
      IF (2.*DIFF/SUM.GT.ZERU)     GO TO 41
   36 CONTINUE

C----              VERIFY OK.


      IF (NFAIL.EQ.0)              GO TO 39
      CALL PRTEST
      NFAIL = 0

   39 NTEST = NTEST + 1
      MTESTV(NTEST) = NTUSE
      IF (NEACHP.EQ.0)       RETURN
      CALL PRTEST
      RETURN

C----              VERIFY FAILURE

   41 JJ = J
      NFAILT = NFAILT + 1
      IF (NFAIL.EQ.0)              GO TO 47
      IF (NFAIL.LT.NFAIPR)         GO TO 48
      NFAIL  = NFAIL  + 1
      GO TO 39

   47 CALL PRTEST
   48 WRITE (ITB,9001) NTUSE
      NFAIL = NFAIL  + 1

      N1 = JJ
#if defined(CERNLIB_QMVAX)
      CALL UCOPY  (HAVE,IHAVE,N)
      CALL UCOPY (AMUST,IMUST,N)
#endif

   51 N2 = MIN (N1+4,NHAVE)
#if defined(CERNLIB_QMVAX)
      WRITE (ITB,9011) N1,(IMUST(J),J=N1,N2)
      WRITE (ITB,9012)    (IHAVE(J),J=N1,N2)
#else
      WRITE (ITB,9011) N1,(AMUST(J),J=N1,N2)
      WRITE (ITB,9012)     (HAVE(J),J=N1,N2)
#endif
      N1= N1+5
      IF (N1.LE.NHAVE)             GO TO 51
      IF (N1-1.NE.NHAVE)  WRITE (ITB,9013)

C--                PRINT VECTORS IN FLOATING FORMAT

      WRITE (ITB,9054) ZERU
      DO 56 J=JJ,NHAVE
      DIFF = ABS (HAVE(J) - AMUST(J))
      SUM  = ABS (HAVE(J) + AMUST(J))
      TEST(J) = 0.
      IF (DIFF .LT. ZERU)          GO TO 56
      TEST(J) = DIFF
      IF (SUM .LT. 2.)             GO TO 56
      TEST(J) = 2.*TEST(J) / SUM
   56 CONTINUE

      N1 = JJ
   60 N2= MIN (N1+4,NHAVE)
      WRITE (ITB,9061) N1,(AMUST(J),J=N1,N2)
      WRITE (ITB,9062)     (HAVE(J),J=N1,N2)
      WRITE (ITB,9063)     (TEST(J),J=N1,N2)
      N1=N1+5
      IF (N1.LE.NHAVE)             GO TO 60
      WRITE(ITB,9013)
      RETURN

   91 CALL PRTEST
      WRITE (ITB,9091) NTUSE, NHAVE
      RETURN

 9001 FORMAT (5H TEST,I5,' FAILED, Dump follows'/)
 9091 FORMAT (1X/' VERIFF VECTOR FOR TEST',I6,'  MORE THAN 200 WORDS,',
     F' NAMELY',I5)

*             FROM MACHINE PATCH
#include "verifmt.inc"

 9013 FORMAT (1X)
 9054 FORMAT (11X,32HFloating point values,   ZERLEV= ,E10.2)
 9061 FORMAT (I10,5H MUST,5E21.13)
 9062 FORMAT (10X,5H HAVE,5E21.13)
 9063 FORMAT (10X,5H TEST,5(E13.5,8X)/1X)
      END
