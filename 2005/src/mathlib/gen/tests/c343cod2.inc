*
* $Id: c343cod2.inc,v 1.1.1.1 1996/04/01 15:01:31 mclareni Exp $
*
* $Log: c343cod2.inc,v $
* Revision 1.1.1.1  1996/04/01 15:01:31  mclareni
* Mathlib gen
*
*
*
* c343cod2.inc
*
    3 CONTINUE
    2 CONTINUE
    1 CONTINUE

      WRITE(LOUT,'(1X)')
      IC=0
      DO 11 IX = 1,2
      IF(IX .EQ. 1) X=HF
      IF(IX .EQ. 2) X=10
      DO 12 IA = 1,3
      IF(IA .EQ. 1) A=0
      IF(IA .EQ. 2) A=HF
      IF(IA .EQ. 3) A=9*R1/10
      DO 13 IN = 1,4
      IF(IN .EQ. 1) N=0
      IF(IN .EQ. 2) N=1
      IF(IN .EQ. 3) N=5
      IF(IN .EQ. 4) N=-5
      IC=IC+1
