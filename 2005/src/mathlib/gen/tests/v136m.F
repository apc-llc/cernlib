*
* $Id: v136m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: v136m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE V136M
C
      CHARACTER*(*) CODE
      PARAMETER (CODE='V136')
C
C     This Subroutine tests the MATHLIB routine V136 RNPSSN
CSEQ,IMP64.
#include "iorc.inc"
C     V136  RNPSSN
 
      LOGICAL LOK
 
      DIMENSION AMU(6),NT(50,6)
 
      DATA AMU /1,5,10,50,87,100/
 
      DATA (NT( 1,L),L=1,6) /   2,   4,  13,  44,  87, 105/
      DATA (NT( 2,L),L=1,6) /   1,   4,  10,  66,  87, 101/
      DATA (NT( 3,L),L=1,6) /   0,   7,  14,  62,  90, 116/
      DATA (NT( 4,L),L=1,6) /   1,   1,   9,  36,  91,  88/
      DATA (NT( 5,L),L=1,6) /   1,   8,  10,  49,  69,  82/
      DATA (NT( 6,L),L=1,6) /   1,   5,   8,  52, 100, 110/
      DATA (NT( 7,L),L=1,6) /   0,   7,   9,  44,  82, 115/
      DATA (NT( 8,L),L=1,6) /   1,   3,  12,  54,  72, 112/
      DATA (NT( 9,L),L=1,6) /   0,   5,  11,  49,  82, 103/
      DATA (NT(10,L),L=1,6) /   1,   9,  10,  45, 101,  99/
      DATA (NT(11,L),L=1,6) /   0,   5,   7,  56,  76,  99/
      DATA (NT(12,L),L=1,6) /   0,   3,  10,  57,  93, 118/
      DATA (NT(13,L),L=1,6) /   2,   4,  10,  50,  80,  89/
      DATA (NT(14,L),L=1,6) /   1,   5,   9,  59,  81, 104/
      DATA (NT(15,L),L=1,6) /   0,   5,   7,  62,  91,  93/
      DATA (NT(16,L),L=1,6) /   0,   5,   8,  61,  82,  95/
      DATA (NT(17,L),L=1,6) /   0,   6,  13,  55,  80, 109/
      DATA (NT(18,L),L=1,6) /   3,   6,  11,  58,  85,  90/
      DATA (NT(19,L),L=1,6) /   2,   6,  10,  42,  80,  98/
      DATA (NT(20,L),L=1,6) /   2,   3,  14,  55,  71,  82/
      DATA (NT(21,L),L=1,6) /   1,   5,   8,  46,  92,  89/
      DATA (NT(22,L),L=1,6) /   0,   5,  16,  41,  80,  96/
      DATA (NT(23,L),L=1,6) /   2,   4,   8,  51,  86,  97/
      DATA (NT(24,L),L=1,6) /   0,   4,   6,  53,  77,  94/
      DATA (NT(25,L),L=1,6) /   1,   8,   4,  45,  81, 106/
      DATA (NT(26,L),L=1,6) /   0,   6,  11,  62,  85,  95/
      DATA (NT(27,L),L=1,6) /   0,   2,   9,  53,  90, 113/
      DATA (NT(28,L),L=1,6) /   0,   4,  10,  44,  85,  88/
      DATA (NT(29,L),L=1,6) /   0,   6,  13,  40,  92, 113/
      DATA (NT(30,L),L=1,6) /   1,   6,   8,  62,  79, 101/
      DATA (NT(31,L),L=1,6) /   0,   5,  10,  54,  79, 100/
      DATA (NT(32,L),L=1,6) /   1,   4,   4,  47,  80,  86/
      DATA (NT(33,L),L=1,6) /   3,   7,   7,  53,  88,  96/
      DATA (NT(34,L),L=1,6) /   0,   5,   7,  58,  82, 102/
      DATA (NT(35,L),L=1,6) /   1,   4,  13,  44,  70,  92/
      DATA (NT(36,L),L=1,6) /   0,   0,   7,  49,  75, 111/
      DATA (NT(37,L),L=1,6) /   0,   3,  14,  39, 104, 103/
      DATA (NT(38,L),L=1,6) /   0,   3,   9,  47,  89, 102/
      DATA (NT(39,L),L=1,6) /   1,   8,  12,  51,  83,  85/
      DATA (NT(40,L),L=1,6) /   1,   5,  11,  40,  89, 114/
      DATA (NT(41,L),L=1,6) /   2,   6,   7,  61,  92, 102/
      DATA (NT(42,L),L=1,6) /   1,   5,   7,  45,  83, 103/
      DATA (NT(43,L),L=1,6) /   1,   5,  11,  58,  98, 110/
      DATA (NT(44,L),L=1,6) /   2,   5,  11,  57,  89,  96/
      DATA (NT(45,L),L=1,6) /   2,   5,  13,  54,  96, 102/
      DATA (NT(46,L),L=1,6) /   0,   1,  11,  49,  81, 119/
      DATA (NT(47,L),L=1,6) /   2,   3,   8,  44,  77, 100/
      DATA (NT(48,L),L=1,6) /   0,   3,  10,  44,  64,  96/
      DATA (NT(49,L),L=1,6) /   1,   6,  13,  40,  76, 103/
      DATA (NT(50,L),L=1,6) /   1,   7,  15,  41,  88, 108/
 
      CALL RLUXGO(3,314159265,0,0)
 
      CALL RNPSET(100.)
 
      LOK=.TRUE.
      DO 2 I = 1,50
      DO 1 MU = 1,6
      CALL RNPSSN(AMU(MU),N,IERR)
    1 LOK=LOK .AND. N .EQ. NT(I,MU)
    2 CONTINUE
 
      CALL RNPSET(60.)
 
      IRC= ITEST(CODE,LOK)
      CALL PAGEND(CODE)
      RETURN
      END
