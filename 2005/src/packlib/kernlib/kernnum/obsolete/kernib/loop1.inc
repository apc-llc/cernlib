*
* $Id: loop1.inc,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: loop1.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_LOOP1_INC
#define CERNLIB_KERNNUM_LOOP1_INC
*
*
* loop1.inc
*
           MACRO
 &LOOP     FORONE    &J
 *         J  =  1
 *   1     CONTINUE
           LA        &J,1
 &LOOP     DS        0H
           MEND
           MACRO
 &EXIT     ENDFOR    &J,&N,&LOOP=
 *         J  =  J + NSTEP
 *         IF(J .LE. N)  GO TO 1
 *   2     CONTINUE
           BXLE      &J,&N-1,&LOOP
 &EXIT     DS        0H
           MEND
           MACRO
 &LABEL    FIRST     &J,&N,&LOOP=
 *         J  =  2
 *         IF(MOD(N,2) .EQ. 0)  GO TO 1
 &LABEL    LA        &J,2
           LA        0,1
           NR        0,&N
           BE        &LOOP
           MEND
           MACRO
 &LOOP     FORTWO    &J,&N,&EXIT=
 *         IF(J .GT. N)  GO TO 2
 *   1     CONTINUE
           CR        &J,&N
           BH        &EXIT
 &LOOP     DS        0H
           MEND

#endif
