*
* $Id: sfinv.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: sfinv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_SFINV_INC
#define CERNLIB_KERNNUM_SFINV_INC
*
*
* sfinv.inc
*
           MACRO
 &LABEL    TSFINV    &N,&A,&IA,&JA,&IA2,&JA2,&X,&Y,&Z,&I,&K
 &LABEL    DS        0H
           LR        &IA2,&IA
           LR        &JA2,&JA
           AR        &IA2,&IA            &IA2 = 2*IA
           AR        &JA2,&JA            &JA2 = 2*JA
 *         DO 20     I  =  2, N
 *            IF(I .EQ. 2)  GOTO 10
 *            CALL UMNS(I-2,A(1,2),A(1,3),A(2,3),A(2,I),A(3,I),
 *                                               A(1,I),A(2,I))
 * 10         A(I-1,I)  =  -A(I-1,I)
 *            CALL VSCL(I-1,A(I,I),A(1,I),A(2,I),A(I,1),A(I,2))
 * 20         CONTINUE
           LA        &I,0                &I = 0 = I-2
           LR        &X,&A               &X = (A(1,1)) = (A(I-1,1))
           LR        &Y,&A               &Y = (A(1,1))
           AR        &Y,&IA              &Y = (A(2,1)) = (A(2,I-1))
           LR        &Z,&A               &Z = (A(1,1)) = (A(1,I-1))
           AR        &A,&JA              &A = (A(1,2))
           LR        &K,&A               &K = (A(1,2)) = (A(I-1,I))
 A&SYSNDX  DS        0H                  I = 2,3,...,N = &I+2
           AR        &Y,&JA              &Y = (A(2,I))
           AR        &Z,&JA              &Z = (A(1,I))
           C         &I,=F'0'
           BC        EQ,B&SYSNDX         IF I .EQ. 2
           TXMPY     UMNS,&I,&I,&A,&IA,&JA,&Y,&IA,&Z,&IA
 B&SYSNDX  DS        0H
           LOAD      0,&K                F0 = A(I-1,I)
           MINUS     0                   F0 = -A(I-1,I)
           STORE     0,&K                A(I-1,I) = -A(I-1,I)
           A         &I,=F'1'            &I = I-1
           AR        &X,&IA              &X = (A(I,1))
           AR        &K,&IA              &K = (A(I,I))
           LOAD      4,&K                F4 = A(I,I)
           TVSCL     &I,&Z,&IA,&X,&JA
           AR        &K,&JA              &K = (A(I,I+1))
           LR        0,&I                R0 = I-1
           A         0,=F'1'             R0 = I
           CR        0,&N
           BC        LT,A&SYSNDX         IF(I .LT. N) REPEAT
           SR        &A,&JA              &A = (A(1,1))
 *         DO 30     K  =  1, N-1
 *            A(K,K)  =  VMPA(N-K,A(K,K+1),A(K,K+2),
 *                                A(K+1,K),A(K+2,K),A(K,K))
 *            CALL MMPY(K,N-K,A(1,K+1),A(1,K+2),A(2,K+1),
 *                                       A(K+1,K+1),A(K+2,K+1),
 *                                       A(1,K+1),A(2,K+1))
 * 30         CONTINUE
 *         CALL MUTL(N,A,A(1,2),A(2,1))
           LA        &K,1                &K = 1 = K
           LR        &X,&A               &X = (A(1,1)) = (A(1,K))
           LR        &Y,&A               &Y = (A(1,1)) = (A(K,K))
 C&SYSNDX  DS        0H
           LR        &Z,&Y               &Z = (A(K,K))
           LOAD      0,&Z                F0 = A(K,K)
           LR        &I,&N               &I = N
           SR        &I,&K               &I = N-K
           AR        &Y,&JA              &Y = (A(K,K+1))
           AR        &Z,&IA              &Z = (A(K+1,K))
           TVMPY2    &I,&Y,&JA,&Z,&IA,&JA2,&IA2
           SR        &Z,&IA              &Z = (A(K,K))
           STORE     0,&Z                A(K,K) = VMPA(N-K,...
           AR        &X,&JA              &X = (A(1,K+1))
           AR        &Y,&IA              &Y = (A(K+1,K+1))
           LR        &Z,&X               &Z = (A(1,K+1))
           TXMPY     MMPY,&K,&I,&X,&IA,&JA,&Y,&IA,&Z,&IA
           A         &K,=F'1'            &K = K+1
           CR        &K,&N
           BC        LT,C&SYSNDX         IF(K+1 .LT. N) REPEAT
           LR        &I,&N               &I = N
           TMUTL     &I,&A,&IA,&JA,&X
           MEND

#endif
