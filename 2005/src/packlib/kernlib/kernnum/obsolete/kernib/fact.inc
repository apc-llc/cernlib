*
* $Id: fact.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: fact.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_FACT_INC
#define CERNLIB_KERNNUM_FACT_INC
*
*
* fact.inc
*
           MACRO
 &LABEL    TFACT     &SIZE,&N,&A,&IA,&JA,&R,&X,&Y,&I,&J,&L
#include "kernnum/global.inc"
           LCLA      &OLD,&A1,&R1,&A1K1,&AK1,&M1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &A1       SETA      &OLD
 &R1       SETA      &A1+1*4
 &A1K1     SETA      &R1+1*4
 &AK1      SETA      &A1K1+1*4
 &M1       SETA      &AK1+1*4
 &STACK    SETA      &M1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     12,'TFACT HAS INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           ST        &A,&A1.(15)         &A1 = (A(1,1))
           ST        &R,&R1.(15)         &R1 = (R(1))
           ST        &A,&AK1.(15)        &AK1 = (A(K,1))
           LR        &J,&A
           AR        &J,&JA              &J = (A(1,K+1))
           ST        &J,&A1K1.(15)       &A1K1 = (A(1,K+1))
 *         DET    =  ONE
 *         JFAIL  =  0
 *         IFAIL  =  0
           LA        &J,ONE&SYSNDX       &J = (ONE)
           LOAD      0,&J                F0 = ONE
           LA        &J,DET&SYSNDX       &J = (DET)
           STORE     0,&J                DET = ONE
           LA        &J,0                &J = 0
           ST        &J,JFL&SYSNDX       JFAIL = 0
           ST        &J,IFL&SYSNDX       IFAIL = 0
 *         M  =  0
           LA        &J,0                &J = 0
           ST        &J,&M1.(15)         &M1 = 0
 *         DO  20    K  =  1, N
           LA        &I,0                &I = 0 = K-1
 A&SYSNDX  DS        0H
 *                                       &I = K-1
 *                                       &A = (A(K,K))
 *                                       &R = (R(K))
 *                                       &A1K1 = (A(1,K+1))
 *                                       &AK1  = (A(K,1))
 *         CALL VMXA(N-K+1,A(K,K),A(K+1,K),L,P)
           LR        &Y,&I               &Y = K-1
           LR        &I,&N               &I = N
           SR        &I,&Y               &I = N-(K-1) = N-K+1
           LA        &I-1,1              ISTEP = 1
           LA        &X,4                &X = 4 = JR
           TVMXA1    &I,&A,&IA,&L,&J     F4 = P
 *         L  =  L + K - 1
           LR        &I,&Y               &I = K-1  AGAIN
           AR        &L,&Y               &L = L+(K-1)
           S         &L,=F'1'            &L = L-1
 *         IF(P    .LE. 0.)     GOTO 90
           LDR       0,4                 F0 = P
           CE        4,=E'0.'
           BC        LE,E&SYSNDX         IF(P .LE. 0.) GOTO 90
 *         IF(L .EQ. K) GOTO 12
           CR        &L,&Y
           BC        EQ,C&SYSNDX         IF(L-1 .EQ. K-1)
 *         CALL VXCH(N,A(K,1),A(K,2),A(L,1),A(L,2))
           L         &X,&AK1.(15)        &X = (A(K,1))
           LR        1,&L                R1 = L-1
           MR        0,&IA               R1 = (L-1)*IA
           L         &J,&A1.(15)         &J = (A(1,1))
           AR        &J,1                &J = (A(L,1))
           LR        &I,&JA
           AR        &I,&JA              &I = 2*JA
           VXCH2     &N,&X,&JA,&J,&JA,&I,&I
           LR        &I,&Y               &I = K-1   AGAIN
 *         M  =  M + 1
 *         R(M) = K*2**12 + L
           L         &J,&M1.(15)         &J = OLD M = M-1
           LA        1,4                 R1 = 4 = JR
           MR        0,&J                R1 = JR*(M-1)
           A         1,&R1.(15)          R1 = (R(M))
           A         &J,=F'1'            &J = OLD M + 1
           ST        &J,&M1.(15)         M = M+1
           LR        &J,1                &J = (R(M))
           LR        0,&I                R0 = K-1
           A         0,=F'1'             R0 = K
           LR        1,&L                R1 = L-1
           A         1,=F'1'             R1 = L
           SLL       1,32-&SIZE
           SLDL      0,&SIZE             R0 = K*2**12 + L
           ST        0,0(&J)             R(M) = K*2**12 + L
 * 12      DET  =  DET * A(K,K)
 *         A(K,K)  =  ONE / A(K,K)
 *         SCALE DETERMINANT, ADD EXPONENT TO JFAIL
 C&SYSNDX  DS        0H
           LOAD      0,&A                F0 = A(K,K)
           LA        &X,DET&SYSNDX       &X = (DET)
           MUL       &X                  F0 = A(K,K)*DET
           STORE     0,&X                DET = A(K,K)*DET
           RCPRCL    &A                  A(K,K)  =  1 / A(K,K)
           L         &L,JFL&SYSNDX       &L = JFAIL
           DET1      &X,&L,&J            INCREMENT JFAIL
           ST        &L,JFL&SYSNDX
 * 14      IF(K .EQ. N)  GOTO 21
 *         IF(K .EQ. 1)  GOTO 16
           LR        &J,&I               &J = K-1
           A         &J,=F'1'            &J = K
           CR        &J,&N
           BC        EQ,F&SYSNDX         IF(K .EQ. N)
           C         &I,=F'0'
           BC        EQ,D&SYSNDX         IF(K-1 .EQ. 0)
 *         CALL MMNA(N-K,K-1,A(1,K+1),JA,IA,A(K,1),JA,A(K,K+1),JA)
           LR        &L,&N               &L = N
           SR        &L,&I               &L = N-(K-1) = N-K+1
           S         &L,=F'1'            &L = N-K
           L         &X,&A1K1.(15)       &X = (A(1,K+1))
           L         &Y,&AK1.(15)        &Y = (A(K,1))
           AR        &A,&JA              &A = (A(K,K+1))
           TXMPY     MMNA,&L,&I,&X,&JA,&IA,&Y,&JA,&A,&JA
           SR        &A,&JA              &A = (A(K,K))
 * 16      CALL VSCL(N-K,A(K,K),A(K,K+1),JA,A(K,K+1),JA)
 *         CALL MMNA(N-K,K,A(K+1,1),IA,JA,A(1,K+1),IA,A(K+1,K+1),IA)
 D&SYSNDX  DS        0H
           LOAD      4,&A                F4 = A(K,K)
           LR        &L,&I               &L = K-1
           A         &L,=F'1'            &L = K
           LR        &I,&N
           SR        &I,&L               &I = N-K
           LA        &I-1,2              ISTEP = 2
           LR        &X,&A               &X = (A(K,K))
           AR        &X,&JA              &X = (A(K,K+1))
           LR        &Y,&X               &Y = (A(K,K+1))
           TVSCL     &I,&X,&JA,&Y,&JA
           SWAP      &I,&L               &I = K
 *                                       &L = N-K
           L         &X,&AK1.(15)        &X = (A(K,1))
           AR        &X,&IA              &X = (A(K+1,1))
           ST        &X,&AK1.(15)        &AK1 = (A(K+1,1))
           L         &Y,&A1K1.(15)       &Y = (A(1,K+1))
           AR        &A,&IA
           AR        &A,&JA              &A = (A(K+1,K+1))
           TXMPY     MMNA,&L,&I,&X,&IA,&JA,&Y,&IA,&A,&IA
           AR        &Y,&JA              &Y = (A(1,K+2))
           ST        &Y,&A1K1.(15)       &A1K1 = (A(1,K+2))
           A         &R,=F'4'            &R = (R(K+1))
           B         A&SYSNDX            REPEAT MAIN LOOP
 IFL&SYSNDX   DS     1F                  IFAIL
 JFL&SYSNDX   DS     1F                  JFAIL
           AIF       ('&T' EQ 'R').RCON
           AIF       ('&T' EQ 'D').DCON
           AIF       ('&T' EQ 'C').CCON
           MEXIT
 .RCON     DS        0F
 ZERO&SYSNDX  DC     E'0.'               ZERO
 ONE&SYSNDX   DC     E'1.'               ONE
 DET&SYSNDX   DS     1F                  DET
           AGO       .ZCON
 .DCON     DS        0D
 ZERO&SYSNDX  DC     D'0.'               ZERO
 ONE&SYSNDX   DC     D'1.'               ONE
 DET&SYSNDX   DS     1D                  DET
           AGO       .ZCON
 .CCON     DS        0F
 ZERO&SYSNDX  DC     E'0.',E'0.'         ZERO
 ONE&SYSNDX   DC     E'1.',E'0.'         ONE
 DET&SYSNDX   DS     2F                  DET
 .ZCON     ANOP
 * 21      R(N)  =  M
 * 90      DET    =  0.
 *         IFAIL  =  -1
 *         JFAIL  =  0
 E&SYSNDX  DS        0H
           CLEAR     0                   F0  =  0.
           L         &I,=F'-1'           &I = -1
           LA        &J,0                &J = 0
           B         Z&SYSNDX            EXIT
 * 21      R(N)  =  M
 *         IF(MOD(M,2) .EQ. 1)  DET = -DET
 *         DE-SCALE DET
 *         RETURN
 F&SYSNDX  L         &A,&A1.(15)         &A = (A)
           L         &R,&R1.(15)         &R = (R)
           LR        1,&N
           S         1,=F'1'             R1 = N-1
           LA        0,4                 R0 = 4 = JR
           MR        0,0                 R1 = (N-1)*JR
           L         &J,&M1.(15)         &J = M
           ST        &J,0(&R,1)          R(N) = M
           LA        &L,DET&SYSNDX       &L = (DET)
           LOAD      0,&L                F0 = DET
           SLL       &J,31               CLEAR ALL BUT BIT 31
           C         &J,=F'0'
           BC        EQ,G&SYSNDX
           MINUS     0                   IF(MOD(M,2) .NE. 0) DET = -DET
 G&SYSNDX  STORE     0,&L
           L         &J,JFL&SYSNDX       &J = JFAIL
           DET2      &L,&J,&I            SCALE DET, SET JFAIL
           L         &I,IFL&SYSNDX       &I = IFAIL
           LOAD      0,&L                F0 = DET
 Z&SYSNDX  DS        0H
 &STACK    SETA      &OLD
           MEND

#endif
