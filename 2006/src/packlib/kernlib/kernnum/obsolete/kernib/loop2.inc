*
* $Id: loop2.inc,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: loop2.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_LOOP2_INC
#define CERNLIB_KERNNUM_LOOP2_INC
*
*
* loop2.inc
*
           MACRO
 &LOOP     LOOPONE   &N
           LA        0,1                 R0 = NSTEP = 1
           LR        1,&N                R1 = N
           LA        &N,1                &N = J = 1
 &LOOP     DS        0H
           MEND
           MACRO
 &EXIT     LEND      &N,&LOOP
           BXLE      &N,0,&LOOP          J=J+1; IF(J .LE. N) GOTO LOOP
 &EXIT     LR        &N,1                &N = N
           MEND
           MACRO
 &LABEL    ONCE      &N,&LOOP
 &LABEL    LA        0,1                 R0 = 1
           LR        1,&N                R1 = N
           LA        &N,2                &N = J = 2
           NR        0,1                 R0 = MOD(N,2)
           BZ        &LOOP-4             IF(MOD(N,2) .EQ. 0) GOTO LOOP
           MEND
           MACRO
 &LOOP     LOOPTWO   &N,&EXIT
           CR        &N,1
           BH        &EXIT               IF(J .GT. N)  GOTO EXIT
           LA        0,2                 R0 = NSTEP = 2
 &LOOP     DS        0H
           MEND

#endif
