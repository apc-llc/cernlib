*
* $Id: tlenoc.F,v 1.1.1.1 1996/02/15 17:54:55 mclareni Exp $
*
* $Log: tlenoc.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:55  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TLENOC

#include "kerngent/mkch.inc"
#include "kerngent/mkcde.inc"

      CHARACTER    LINE*80, COL(80)*1
      EQUIVALENCE (LINE,COL,MKLINE)

C-----------       LNBLNK

      CALL NEWGUY ('LNBLNK - LENOCC.','TLENOC  ')

      IA(1) = 0
      LINE  = 'alphabeta'
      NTN   = 1

      DO  24  JA=1,11
      DO  24  JE=JA,80
      MUST = MAX (MIN(JE,9)-JA+1,0)
      NOCC = LNBLNK(LINE(JA:JE))
      IF (NOCC.NE.MUST)            GO TO 29
   24 CONTINUE
      CALL MVERII (NTN,IA,IA,1)

      NTN   = 2
      DO  26  JA=1,11
      LINE  = 'alphabeta'
      DO  26  JE=40,79
      COL(JE-JA) = 'x'
      COL(JE+1)  = 'y'
      MUST = JE+1 - 2*JA
      NOCC = LNBLNK(LINE(JA:JE))
      IF (NOCC.NE.MUST)            GO TO 29
      COL(JE+1)  = ' '
   26 CONTINUE
      CALL MVERII (NTN,IA,IA,1)
      GO TO 31

   29 CALL MVERII (NTN,NOCC,MUST,1)
      WRITE (ITB,9029)  JA,JE,MUST,NOCC
 9029 FORMAT (' Test of LNBLNK failed : JA,JE,MUST,HAVE =',4I6/)

C-----------       LENOCC

   31 IA(1) = 0
      LINE  = 'alphabeta'
      NTN   = 3

      DO  34  JA=1,11
      DO  34  JE=JA,80
      MUST = MAX (MIN(JE,9)-JA+1,0)
      NOCC = LENOCC(LINE(JA:JE))
      IF (NOCC.NE.MUST)            GO TO 39
   34 CONTINUE
      CALL MVERII (NTN,IA,IA,1)

      NTN   = 4
      DO  36  JA=1,11
      LINE  = 'alphabeta'
      DO  36  JE=40,79
      COL(JE-JA) = 'x'
      COL(JE+1)  = 'y'
      MUST = JE+1 - 2*JA
      NOCC = LENOCC(LINE(JA:JE))
      IF (NOCC.NE.MUST)            GO TO 39
      COL(JE+1)  = ' '
   36 CONTINUE
      CALL MVERII (NTN,IA,IA,1)
      GO TO 69

   39 CALL MVERII (NTN,NOCC,MUST,1)
      WRITE (ITB,9039)  JA,JE,MUST,NOCC
 9039 FORMAT (' Test of LENOCC failed : JA,JE,MUST,HAVE =',4I6/)

   69 IF (ITIMES.EQ.0)             RETURN

C--      TIMING

      NLOOP  = 4*ITIMES
      NTIMES = NLOOP*1000
      LINE   = 'rho'

      DO 76  JE= 80,3,-9

      CALL TIMED (TIMERD)
      NLOOPX = NLOOP
   72 JT = 0
   73 JT = JT + 1
      IA(JT) = LNBLNK (LINE(1:JE))
      IF (JT.NE.1000)              GO TO 73
      NLOOPX = NLOOPX - 1
      IF (NLOOPX.NE.0)             GO TO 72
      CALL TIME77 (NTIMES,'LNBLNK',JE,'chars')

      CALL TIMED (TIMERD)
      NLOOPX = NLOOP
   74 JT = 0
   75 JT = JT + 1
      IA(JT) = LENOCC (LINE(1:JE))
      IF (JT.NE.1000)              GO TO 75
      NLOOPX = NLOOPX - 1
      IF (NLOOPX.NE.0)             GO TO 74
      CALL TIME77 (NTIMES,'LENOCC',JE,'chars')
   76 CONTINUE
      RETURN
      END
