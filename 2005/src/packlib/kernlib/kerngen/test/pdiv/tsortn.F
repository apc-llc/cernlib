*
* $Id: tsortn.F,v 1.1.1.1 1996/02/15 17:54:56 mclareni Exp $
*
* $Log: tsortn.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:56  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TSORTN

#include "kerngent/mkcde.inc"

      DIMENSION    IMAT(100),       AMAT(100)
      EQUIVALENCE (IMAT(1),IB(1)), (AMAT(1),B(101))

C     DOUBLE PRECISION   DMAT(100)
C     EQUIVALENCE       (DMAT(1),IB(201))


      CALL VZERO (IA,1000)
      CALL UCOPY (INTG,IA,100)

      DO  14  J=1,99,4
   14 IA(J) = J/16 - 2

      CALL UCOPY (INTG(2),IA(101),10)
      IA(111)= 12
      CALL UCOPY (INTG(12),IA(112),9)

      DO 18 I= 1,20
      J=21-I
   18 IA(I+120) = J

      CALL NEWGUY ('SORTI/R/D.','TSORTN  ')

#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9801) (IA(J),J=1,100)
      IF (LOGLEV.NE.0)  WRITE (ITB,9802) (IA(J),J=101,120)
      IF (LOGLEV.NE.0)  WRITE (ITB,9803) (IA(J),J=121,140)
 9801 FORMAT (' IA(1:100) is :'/1X/25(4X,4I4/))
 9802 FORMAT (' IA(101:120) is :'/1X/2(4X,10I4/))
 9803 FORMAT (' IA(121:140) is :'/1X/2(4X,10I4/))
#endif

C----              Test SORTI normal

      CALL UCOPY (IA(65), IMAT(1),  36)
      CALL UCOPY (IA(49), IMAT(37), 16)
      CALL UCOPY (IA(1),  IMAT(53), 48)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9810) (IMAT(J),J=1,100)
 9810 FORMAT (' 11/1 bef > IMAT(1:100) is :'/1X/25(4X,4I4/))
#endif

      CALL SORTI (IMAT, 4, 25, 1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9811) (IMAT(J),J=1,100)
 9811 FORMAT (' 11/1 aft > IMAT(1:100) is :'/1X/25(4X,4I4/))
#endif

      CALL MVERII (11,IMAT,IA,100)

      CALL UCOPY (IA(65), IMAT(1),  36)
      CALL UCOPY (IA(49), IMAT(37), 16)
      CALL UCOPY (IA(1),  IMAT(53), 48)
      CALL SORTI (IMAT, 4, 25, -1)
#if defined(CERNLIB_DEVMIK)
      CALL PRTEST
      IF (LOGLEV.NE.0)  WRITE (ITB,9812) (IMAT(J),J=1,100)
 9812 FORMAT (' 12/1 aft > IMAT(1:100) is :'/1X/25(4X,4I4/))
#endif

      CALL SORTI (IMAT, 4, 25, 1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9813) (IMAT(J),J=1,100)
 9813 FORMAT (' 12/2 aft > IMAT(1:100) is :'/1X/25(4X,4I4/))
#endif

      CALL MVERII (12,IMAT,IA,100)

      CALL UCOPY (INTG, IMAT, 20)
      IMAT(1) = 12
#if defined(CERNLIB_DEVMIK)
      CALL PRTEST
      IF (LOGLEV.NE.0)  WRITE (ITB,9814) (IMAT(J),J=1,20)
 9814 FORMAT (' 13/1 bef > IMAT(1:20) is :'/1X/2(4X,10I4/))
#endif

      CALL SORTI (IMAT,10, 2, 1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9815) (IMAT(J),J=1,20)
 9815 FORMAT (' 13/2 aft > IMAT(1:20) is :'/1X/2(4X,10I4/))
#endif

      CALL SORTI (IMAT, 1,20, 1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9816) (IMAT(J),J=1,20)
 9816 FORMAT (' 13/3 aft > IMAT(1:20) is :'/1X/2(4X,10I4/))
#endif

      CALL MVERII (13,IMAT,IA(101),20)

      CALL UCOPY (INTG,IMAT,20)
#if defined(CERNLIB_DEVMIK)
      CALL PRTEST
      IF (LOGLEV.NE.0)  WRITE (ITB,9817) (IMAT(J),J=1,20)
 9817 FORMAT (' 14/1 bef > IMAT(1:20) is :'/1X/2(4X,10I4/))
#endif

      CALL SORTI (IMAT, 1,20,-1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9818) (IMAT(J),J=1,20)
 9818 FORMAT (' 14/1 aft > IMAT(1:20) is :'/1X/2(4X,10I4/))
#endif

      CALL MVERII (14,IMAT,IA(121),20)

C----              TEST FAULTY CALLS
C     CALL UCOPY (IA,IMAT,20)
C     CALL SORTI (IMAT, 4, 5, 0)
C     CALL SORTI (IMAT, 4, 5,100)
C     CALL SORTI (IMAT, 4, 0,1)
C     CALL MVERII (15,IMAT,INTG  ,20)

C----              Test SORTR normal

      DO 32  J=1,140
   32 A(J+200) = IA(J)

      CALL UCOPY (A(265), AMAT(1),  36)
      CALL UCOPY (A(249), AMAT(37), 16)
      CALL UCOPY (A(201), AMAT(53), 48)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9820) (AMAT(J),J=1,100)
 9820 FORMAT (' 21/1 bef > AMAT(1:100) is :'/1X/25(4X,4F5.0/))
#endif

      CALL SORTR (AMAT, 4, 25, 1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9821) (AMAT(J),J=1,100)
 9821 FORMAT (' 21/1 aft > AMAT(1:100) is :'/1X/25(4X,4F5.0/))
#endif

      CALL MVERIF (21,AMAT,A(201),100)

      CALL UCOPY (A(265), AMAT(1),  36)
      CALL UCOPY (A(249), AMAT(37), 16)
      CALL UCOPY (A(201), AMAT(53), 48)
      CALL SORTR (AMAT, 4, 25, -1)
#if defined(CERNLIB_DEVMIK)
      CALL PRTEST
      IF (LOGLEV.NE.0)  WRITE (ITB,9822) (AMAT(J),J=1,100)
 9822 FORMAT (' 22/1 aft > AMAT(1:100) is :'/1X/25(4X,4F5.0/))
#endif

      CALL SORTR (AMAT, 4, 25, 1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9823) (AMAT(J),J=1,100)
 9823 FORMAT (' 22/2 aft > AMAT(1:100) is :'/1X/25(4X,4F5.0/))
#endif

      CALL MVERIF (22,AMAT,A(201),100)

      CALL VFLOAT (INTG, AMAT, 20)
      AMAT(1) = 12.
#if defined(CERNLIB_DEVMIK)
      CALL PRTEST
      IF (LOGLEV.NE.0)  WRITE (ITB,9824) (AMAT(J),J=1,20)
 9824 FORMAT (' 23/1 bef > AMAT(1:20) is :'/1X/2(4X,10F5.0/))
#endif

      CALL SORTR (AMAT,10, 2, 1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9825) (AMAT(J),J=1,20)
 9825 FORMAT (' 23/2 aft > AMAT(1:20) is :'/1X/2(4X,10F5.0/))
#endif

      CALL SORTR (AMAT, 1,20, 1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9826) (AMAT(J),J=1,20)
 9826 FORMAT (' 23/3 aft > AMAT(1:20) is :'/1X/2(4X,10F5.0/))
#endif

      CALL MVERIF (23,AMAT,A(301),20)

      CALL VFLOAT (INTG,AMAT,20)
#if defined(CERNLIB_DEVMIK)
      CALL PRTEST
      IF (LOGLEV.NE.0)  WRITE (ITB,9827) (AMAT(J),J=1,20)
 9827 FORMAT (' 24/1 bef > AMAT(1:20) is :'/1X/2(4X,10F5.0/))
#endif

      CALL SORTR (AMAT, 1,20,-1)
#if defined(CERNLIB_DEVMIK)
      IF (LOGLEV.NE.0)  WRITE (ITB,9828) (AMAT(J),J=1,20)
 9828 FORMAT (' 24/1 aft > AMAT(1:20) is :'/1X/2(4X,10F5.0/))
#endif

      CALL MVERIF (24,AMAT,A(321),20)

C----              TEST FAULTY CALLS
C     CALL UCOPY (IA,AMAT,20)
C     CALL SORTR (AMAT, 4, 5, 0)
C     CALL SORTR (AMAT, 4, 5,100)
C     CALL SORTR (AMAT, 4, 0,1)
C     CALL MVERIF (25,AMAT,INTG  ,20)

C--      TIMING
      IF (ITIMES.EQ.0)       RETURN
      NTIMES = ITIMES*200
      CALL UCOPY (INTG,IMAT,100)
      CALL TIMED (TIMERD)

      DO 72 J= 1,NTIMES
      CALL SORTI (IMAT,4,25,-1)
      CALL SORTI (IMAT,4,25, 1)
   72 CONTINUE
      CALL TIME77 (NTIMES,'SORTI ',200,'words')

      CALL VFLOAT (INTG,AMAT,100)
      CALL TIMED (TIMERD)

      DO 74 J= 1,NTIMES
      CALL SORTR (AMAT,4,25,-1)
      CALL SORTR (AMAT,4,25, 1)
   74 CONTINUE
      CALL TIME77 (NTIMES,'SORTR ',200,'words')
      RETURN
      END
