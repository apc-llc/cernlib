*
* $Id: stack.inc,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: stack.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_STACK_INC
#define CERNLIB_KERNNUM_STACK_INC
*
*
* stack.inc
*
           MACRO
 &LABEL    PUSH      &R
#include "kernnum/global.inc"
           AIF       (&STACK LT &STKLIM).ALPHA
           MNOTE     12,'STACK OVERFLOW'
           MEXIT
 .ALPHA    ANOP
 &LABEL    ST        &R,&STACK.(&BASEREG)
 &STACK    SETA      &STACK+1*4
           MEND
           MACRO
 &LABEL    POP       &R
#include "kernnum/global.inc"
 &STACK    SETA      &STACK-1*4
           AIF       (&STACK GE &STKORG).ALPHA
           MNOTE     12,'STACK UNDERFLOW'
           MEXIT
 .ALPHA    ANOP
 &LABEL    L         &R,&STACK.(&BASEREG)
           MEND

#endif
