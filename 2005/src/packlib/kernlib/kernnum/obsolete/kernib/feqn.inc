*
* $Id: feqn.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: feqn.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_FEQN_INC
#define CERNLIB_KERNNUM_FEQN_INC
*
*
* feqn.inc
*
           MACRO
 &LABEL    TFEQN1    &N,&A,&IA,&JA,&K,&B,&IA2,&JA2,&Z,&I
#include "kernnum/global.inc"
           LCLA      &OLD,&A1,&B1,&Z1,&K1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &A1       SETA      &OLD
 &B1       SETA      &A1+1*4
 &K1       SETA      &B1+1*4
 &STACK    SETA      &K1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     12,'TFEQN1 HAS INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
 *         DO  3     L  =  1, K
 *                   B(1,L)  =  A(1,1) * B(1,L)
 *                   IF(N .EQ. 1)  GOTO 3
 *                   DO 1 I = 1, N-1
 *                        B(I+1,L) = -VMPY(I,A(I+1,1),"JA",B(1,L),"IA",
 *                                           -B(I+1,L)) * A(I+1,I+1)
 *  1                     CONTINUE
 *                   IA  =  -IA
 *                   JA  =  -JA
 *                   DO 2 I = 1, N-1
 *              B(N-I,L) = -VMPY(I,A(N-I,N),"JA",B(N,L)"IA",-B(N-I,L))
 *  2                     CONTINUE
 *                   IA  =  -IA
 *                   JA  =  -JA
 *  3                CONTINUE
           ST        &K,&K1.(15)         SAVE K
           LR        &IA2,&IA
           LR        &JA2,&JA
           AR        &IA2,&IA            &IA2 = 2*IA
           AR        &JA2,&JA            &JA2 = 2*JA
 A&SYSNDX  DS        0H
 *                                       &A = (A(1,1))
 *                                       &B = (B(1,L))
           LOAD      0,&A                F0 = A(1,1)
           MUL       &B                  F0 = A(1,1)*B(1,L)
           STORE     0,&B                B(1,L) = A(1,1)*B(1,L)
           LA        &I,1                &I = 1
           CR        &N,&I
           BC        EQ,D&SYSNDX         IF N = 1
           LR        &Z,&B               &Z = (B(1,L))
           ST        &B,&B1.(15)         &B1= (B(1,L))
 *                                       &A = (A(I,1))
 *                                       &B = (B(1,L))
 *                                       &Z = (B(I,L))
 B&SYSNDX  DS        0H
           AR        &A,&IA              &A = (A(I+1,1))
           AR        &Z,&IA              &Z = (B(I+1,L))
           ST        &A,&A1.(15)         &A1= (A(I+1,1))
           LOAD      0,&Z
           MINUS     0                   F0 = -(B(I+1,L)
           VMPY      &I,&A,&JA,&B,&IA,&JA2,&IA2
 *                                       &A = (A(I+1,I+1))
 *                                       &B = (B(I+1,L))
           MINUS     0
           MUL       &A
           STORE     0,&B                B(I+1,L)=-VMPY(...)*A(I+1,I+1)
           LR        1,&A                SAVE (A(I+1,I+1)) IN R1
           L         &A,&A1.(15)         &A = (A(I+1,1))
           L         &B,&B1.(15)         &B = (B(1,L))
           A         &I,=F'1'            &I = I+1
           CR        &I,&N
           BC        LT,B&SYSNDX         IF &I .LT. N
 *                                       R1 = (A(N,N))
 *                                       &Z = (B(N,L))
           LCR       &IA,&IA             IA = -IA
           LCR       &IA2,&IA2
           LCR       &JA,&JA             JA = -JA
           LCR       &JA2,&JA2
           LR        &A,1                &A = (A(N,N))
           LR        &B,&Z               &B = (B(N,L))
           LA        &I,1                &I = 1
           ST        &B,&B1.(15)         &B1= (B(N,L))
 C&SYSNDX  DS        0H
 *                                       &A = (A(N-I+1,N))
 *                                       &B = (B(N,L))
 *                                       &Z = (B(N-I+1,L))
           AR        &A,&IA              &A = (A(N-I,N))
           ST        &A,&A1.(15)         &A1= (A(N-I,N))
           AR        &Z,&IA              &Z = (B(N-I,L))
           LOAD      0,&Z
           MINUS     0                   F0 = -B(N-I,L)
           VMPY      &I,&A,&JA,&B,&IA,&JA2,&IA2
 *                                       &A = (A(N-I,N-I))
 *                                       &B = (B(N-I,L))
           MINUS     0
           STORE     0,&B                B(N-I,L) = -VMPA(...)
           LR        0,&A                SAVE (A(N-I,N-I)) IN R0
           L         &A,&A1.(15)         &A = (A(N-I,N))
           L         &B,&B1.(15)         &B = (B(N,L))
           A         &I,=F'1'            &I = I+1
           CR        &I,&N
           BC        LT,C&SYSNDX         IF(I .LT. N)
 *                                       R0 = (A(1,1))
 *                                       &Z = (B(1,L))
           LCR       &IA,&IA             IA = -IA
           LCR       &IA2,&IA2
           LCR       &JA,&JA             JA = -JA
           LCR       &JA2,&JA2
           LR        &A,0                &A = (A(1,1))
           LR        &B,&Z               &B = (B(1,L))
 D&SYSNDX  DS        0H
           AR        &B,&JA              &B = (B(1,L+1))
           S         &K,=F'1'
           BC        NE,A&SYSNDX         IF(L .LT. K)
 &STACK    SETA      &OLD
           L         &K,&K1.(15)         RESTORE &K = K
           MEND
           MACRO
 &LABEL    TFEQN     &SIZE,&N,&A,&IA,&JA,&R,&K,&B,&IA2,&JA2,&Z,&I,&J
#include "kernnum/global.inc"
 &LABEL    PUSH      &A
           LR        &JA2,&JA
           AR        &JA2,&JA            &JA2 = 2 * JA
           LR        &I,&K               &I = K
           LR        1,&N
           S         1,=F'1'             R1 = N-1
           LA        &A,4                &A = 4 = JR
           MR        0,&A                R1 = (N-1) * JR
           L         &K,0(&R,1)          &K = R(N) = L
           C         &K,=F'0'
           BC        LE,A&SYSNDX         IF(L .LE. 0)
           PUSH      &R                  SAVE (R(1))
           MSUBST    &SIZE,&I,&B,&IA,&JA,&K,&R,&A,&JA2,&Z,&IA2
           POP       &R                  &R = (R(1))
 A&SYSNDX  POP       &A                  &A = (A(1,1))
           LR        &K,&I               &K = K
           TFEQN1    &N,&A,&IA,&JA,&K,&B,&IA2,&JA2,&Z,&I
           MEND

#endif
