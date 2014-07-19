*
* $Id: finv.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: finv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_FINV_INC
#define CERNLIB_KERNNUM_FINV_INC
*
*
* finv.inc
*
           MACRO
 &LABEL    TFINV1    &N,&A,&IA,&JA,&X,&Y,&Z,&I,&K
 &LABEL    DS        0H
           LR        &X,&A               &X = (A(1,1))
           LR        &Y,&A               &Y = (A(I-1,1))
           LR        &Z,&A               &Z = (A(I-1,1))
           LR        &K,&A               &K = (A(I-1,I-1))
           LA        &I,1                &I = 1 = I-1
 A&SYSNDX  DS        0H
           AR        &Y,&IA              &Y = (A(I,1))
           AR        &Z,&IA              &Z = (A(I,1))
           AR        &K,&IA
           AR        &K,&JA              &K = (A(I,I))
           TXMPY     UMPY,&I,&I,&X,&JA,&IA,&Y,&JA,&Z,&JA
           LOAD      4,&K                F4 = A(I,I)
           MINUS     4                   F4 = -A(I,I)
           TVSCL     &I,&Y,&JA,&Z,&JA
           A         &I,=F'1'            &I = I+1
           CR        &I,&N
           BC        LT,A&SYSNDX         IF &I .LT. N REPEAT
           LR        &X,&A               &X = (A(1,1))
           AR        &X,&JA              &X = (A(1,2))
           LOAD      0,&X                F0 = A(1,2)
           MINUS     0                   F0 = -A(1,2)
           STORE     0,&X                A(1,2) = -A(1,2)
           C         &N,=F'2'
           BC        EQ,C&SYSNDX         IF N .EQ. 2
           LR        &Y,&X
           AR        &Y,&IA              &Y = (A(2,I-1))
           LR        &Z,&X               &Z = (A(1,I-1))
           LR        &K,&Z               &K = (A(I-2,I-1))
           S         &N,=F'2'            &N = N-2
           LA        &I,1                &I = 1 = I-2
 B&SYSNDX  DS        0H
           AR        &Y,&JA              &Y = (A(2,I))
           AR        &Z,&JA              &Z = (A(1,I))
           AR        &K,&IA
           AR        &K,&JA              &K = (A(I-1,I))
           TXMPY     UMNS,&I,&I,&X,&IA,&JA,&Y,&IA,&Z,&IA
           LOAD      0,&K                F0 = A(I-1,I)
           MINUS     0                   F0 = -A(I-1,I)
           STORE     0,&K                A(I-1,I) = -A(I-1,I)
           A         &I,=F'1'            &I = I+1
           CR        &I,&N
           BC        LE,B&SYSNDX         IF(I .LE. N-2) REPEAT
           A         &N,=F'2'            &N = N
 C&SYSNDX  DS        0H
           LR        &X,&A
           AR        &X,&IA              &X = (A(I+1,1))
           LR        &Y,&A
           AR        &Y,&JA              &Y = (A(I,I+1))
           LR        &Z,&A               &Z = (A(I,1))
           LR        &I,&N
           S         &I,=F'1'            &I = N-I
 D&SYSNDX  DS        0H
           LR        &K,&N               &K = N
           SR        &K,&I               &K = N-(N-I) = I
           TXMPY     MMPA,&K,&I,&X,&JA,&IA,&Y,&JA,&Z,&JA
           LR        &K,&X               SAVE (A(I+1,1)) IN &K
           LR        &Z,&Y               &Z = (A(I,I+1))
           LR        &X,&Y
           AR        &X,&IA              &X = (A(I+1,I+1))
           TXMPY     UMPY,&I,&I,&X,&JA,&IA,&Y,&JA,&Z,&JA
           LR        &X,&K
           AR        &X,&IA              &X = (A(I+2,1))
           AR        &Y,&IA
           AR        &Y,&JA              &Y = (A(I+1,I+2))
           LR        &Z,&K               &Z = (A(I+1,1))
           S         &I,=F'1'            &I = &I-1
           BC        GT,D&SYSNDX         IF(N-I .GT. 0) REPEAT
           MEND
           MACRO
 &LABEL    TFINV     &SIZE,&N,&A,&IA,&JA,&R,&IA2,&JA2,&X,&Y,&Z,&I
#include "kernnum/global.inc"
           LCLA      &OLD,&R1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &R1       SETA      &OLD
 &STACK    SETA      &R1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     12,'TFINV HAS INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           C         &N,=F'1'
           BC        LE,A&SYSNDX         IF(N .LE. 1) ALL DONE
           ST        &R,&R1.(15)         &R1 = (R)
           TFINV1    &N,&A,&IA,&JA,&X,&Y,&Z,&I,&R
           L         &R,&R1.(15)         &R = (R)
           LA        1,4                 R1 = JR = 4
           MR        0,&N                R1 = JR*N
           S         1,=F'4'             R1 = JR*(N-1)
           L         &Z,0(&R,1)          &Z = R(N) = L
           C         &Z,=F'0'
           BC        LE,A&SYSNDX         IF(L .LE. 0) NO COLUMN SUBST.
           L         &JA2,=F'-4'         &JA2 = -JR
           LA        1,4                 R1 = JR
           MR        0,&Z                R1 = JR*L
           S         1,=F'4'             R1 = JR*(L-1)
           AR        &R,1                &R = (R(L))
           LR        &IA2,&IA
           AR        &IA2,&IA            &IA2 = 2*IA
           MSUBST    &SIZE,&N,&A,&JA,&IA,&Z,&R,&JA2,&IA2,&X,&Y
 A&SYSNDX  DS        0H
 &STACK    SETA      &OLD
           MEND

#endif
