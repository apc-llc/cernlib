*
* $Id: utl.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: utl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_UTL_INC
#define CERNLIB_KERNNUM_UTL_INC
*
*
* utl.inc
*
           MACRO
 &LABEL    TMUTL     &N,&X,&IX,&JX,&Z    (&Z IS TEMPORARY)
#include "kernnum/global.inc"
 &LABEL    LR        1,&N                R1 = N
           LA        0,1                 R0 = 1
           SR        1,0                 R1 = N-1
           BZ        D&SYSNDX            IF(N .EQ. 1) EXIT
           PUSH      &N                  STACK N
           PUSH      &X                  STACK (X(1,1))
 A&SYSNDX  LR        &Z,&X               &Z = (X(I,I))
           AR        &X,&JX              &X = (X(I,I+1))
           AR        &Z,&IX              &Z = (X(I+1,I))
           CPY       &X,&Z
           SR        1,0                 R1 = N-2
           BZ        C&SYSNDX            IF(I+1 .EQ. N)
           AR        &X,&JX              &X = (X(I,I+2))
           AR        &Z,&IX              &Z = (X(I+2,I))
           PUSH      &X                  STACK (X(I,I+2))
           LA        &N,1                &N = J = 1
 B&SYSNDX  CPY       &X,&Z
           CPY       &X,&Z,JX=&IX,JZ=&JX
           AR        &X,&JX
           AR        &Z,&IX
           BXLE      &N,0,B&SYSNDX
           POP       &X                  &X = (X(I,I+2))
           AR        &X,&IX
           AR        &X,&IX              &X = (X(I+2,I+2))
           SR        1,0                 R1 = N-3 ETC
           BNZ       A&SYSNDX
 C&SYSNDX  POP       &X                  &X = (X(1,1))
           POP       &N                  &N = N
 D&SYSNDX  DS        0H
           MEND

#endif
