*
* $Id: mverii.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: mverii.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE MVERII (NTT,IHAVE,IMUST,NN)

#include "kerngent/mkch.inc"
#include "kerngent/mkcde.inc"

      DIMENSION IHAVE(99),IMUST(99), NTT(9), NN(9)

      NTUSE = NTT(1)
      NHAVE = NN(1)
      IF (NHAVE.GE.201)            GO TO 91
      IF (NTEST.GE.10)  CALL PRTEST

#if (defined(CERNLIB_GENMUST))&&(!defined(CERNLIB_GENUSE))
C----              Generate the MUST data on "cards"

      IF (IFLGU.EQ.0)              GO TO 31
      IF (LUNGU.EQ.0)  LUNGU= ITB

      MINEL = 0
      MAXEL = 0
      DO 22  J=1,NHAVE
      MINEL = MIN (MINEL, IHAVE(J))
   22 MAXEL = MAX (MAXEL, IHAVE(J))
      IF (MINEL.LT.0)  MINEL= -10*MINEL
      MAXEL = MAX (MINEL,MAXEL)

      NCHEL = 1
      DO 23  J=1,11
      IF (MAXEL.LT.10)       GO TO 24
      NCHEL = NCHEL + 1
   23 MAXEL = MAXEL/10
   24 NCHEL = NCHEL + 2
      JEL   = 0

      WRITE (LUNGU,9024) NTUSE
 9024 FORMAT (' DATA for test',I6)

   25 MKLINE(1:80) = '     +'
      JCOLE = 8
   26 JEL   = JEL + 1
      JCOLF = JCOLE + 1
      JCOLE = JCOLE + NCHEL
      CALL CSETDI (IHAVE(JEL), MKLINE,JCOLF,JCOLE-1)
      IF (JEL.LT.NHAVE)  THEN
          JCOLPR = JCOLE
          MKCOL(JCOLPR) = ','
        ELSE
          JCOLPR = JCOLE + 1
          MKCOL(JCOLPR) = '/'
          JCOLE = 80
        ENDIF
        IF (JCOLE+NCHEL.LT.73)     GO TO 26

      WRITE (LUNGU,9025) MKLINE(1:JCOLPR)
 9025 FORMAT(A)

      IF (JEL.LT.NHAVE)            GO TO 25
      RETURN
#endif

C----              Verify

   31 DO 34 JJ=1,NHAVE
      J= JJ
      IF  (IHAVE(J).NE.IMUST(J))   GO TO 41
   34 CONTINUE

C--                verify ok.

      IF (NFAIL.EQ.0)              GO TO 39
      CALL PRTEST
      NFAIL = 0

   39 NTEST = NTEST + 1
      MTESTV(NTEST) = NTUSE
      IF (NEACHP.EQ.0)             RETURN
      CALL PRTEST
      RETURN

C--                verify failure

   41 N1     = J
      NFAILT = NFAILT + 1
      IF (NFAIL.EQ.0)              GO TO 47
      IF (NFAIL.LT.NFAIPR)         GO TO 48
      NFAIL = NFAIL + 1
      GO TO 39

   47 CALL PRTEST
   48 WRITE (ITB,9001) NTUSE
      NFAIL = NFAIL  + 1

   51 N2= MIN (N1+4,NHAVE)
      WRITE (ITB,9011) N1,(IMUST(J),J=N1,N2)
      WRITE (ITB,9012) (IHAVE(J),J=N1,N2)
      N1= N1+5
      IF (N1.LE.NHAVE)             GO TO 51
      IF (N1-1.NE.NHAVE)  WRITE (ITB,9013)
      RETURN

   91 CALL PRTEST
      WRITE (ITB,9091) NTUSE, NHAVE
      RETURN

 9001 FORMAT (5H TEST,I5,' FAILED, Hex dump follows'/)
 9091 FORMAT (1X/' Verify vector for test',I6,'  more than 200 words,',
     F' NAMELY',I5)

*             from machine patch
#include "verifmt.inc"

 9013 FORMAT (1X)
      END
