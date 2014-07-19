*
* $Id: sfact.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: sfact.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_SFACT_INC
#define CERNLIB_KERNNUM_SFACT_INC
*
*
* sfact.inc
*
           MACRO
 &LABEL    TSFACTD   &N,&A,&IA,&JA,&X,&Y,&I,&J,&L
#include "kernnum/global.inc"
           LCLA      &OLD,&A1,&A1K1,&AK1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &A1       SETA      &OLD
 &A1K1     SETA      &A1+1*4
 &AK1      SETA      &A1K1+1*4
 &STACK    SETA      &AK1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     12,'TSFACTD HAS INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           ST        &A,&A1.(15)         &A1 = (A(1,1))
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
 *         DO  20    K  =  1, N
           LA        &I,0                &I = 0 = K-1
 A&SYSNDX  DS        0H
 *                                       &I = K-1
 *                                       &A = (A(K,K))
 *                                       &A1K1 = (A(1,K+1))
 *                                       &AK1  = (A(K,1))
 *         IF(A(K,K) .LE. 0.)     GOTO 90
 *         DET  =  DET * A(K,K)
 *         A(K,K)  =  ONE / A(K,K)
 *         SCALE DETERMINANT, ADD EXPONENT TO JFAIL
           LOAD      0,&A                F0 = A(K,K)
           CE        0,=E'0.'
           BC        LE,E&SYSNDX         IF(A(K,K) .LE. 0.) GOTO 90
           LA        &X,DET&SYSNDX       &X = (DET)
           MUL       &X                  F0 = A(K,K)*DET
           STORE     0,&X                DET = A(K,K)*DET
           RCPRCL    &A                  A(K,K)  =  1 / A(K,K)
           L         &L,JFL&SYSNDX       &L = JFAIL
           DET1      &X,&L,&J            INCREMENT JFAIL
           ST        &L,JFL&SYSNDX
 * 14      IF(K .EQ. N)  GOTO 21
           LR        &J,&I               &J = K-1
           A         &J,=F'1'            &J = K
           CR        &J,&N
           BC        EQ,F&SYSNDX         IF(K .EQ. N)
 *         CALL VSCL(N-K,A(K,K),A(K+1,K),IA,A(K,K+1),JA)
 *         CALL MMNA(N-K,K,A(K+1,1),IA,JA,A(1,K+1),IA,A(K+1,K+1),IA)
 D&SYSNDX  DS        0H
           LOAD      4,&A                F4 = A(K,K)
           LR        &L,&I               &L = K-1
           A         &L,=F'1'            &L = K
           LR        &I,&N
           SR        &I,&L               &I = N-K
           LR        &X,&A               &X = (A(K,K))
           LR        &Y,&X               &Y = (A(K,K))
           AR        &Y,&JA              &Y = (A(K,K+1))
           AR        &X,&IA              &X = (A(K+1,K))
           TVSCL     &I,&X,&IA,&Y,&JA
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
 * 90      DET    =  0.
 *         IFAIL  =  -1
 *         JFAIL  =  -2
 E&SYSNDX  DS        0H
           CLEAR     0                   F0  =  0.
           L         &I,=F'-1'           &I = -1
           L         &J,=F'-2'           &J = -2
           B         Z&SYSNDX            EXIT
 * 21      DE-SCALE DET
 *         RETURN
 F&SYSNDX  L         &A,&A1.(15)         &A = (A)
           LA        &L,DET&SYSNDX       &L = (DET)
           L         &J,JFL&SYSNDX       &J = JFAIL
           DET2      &L,&J,&I            SCALE DET, SET JFAIL
           L         &I,IFL&SYSNDX       &I = IFAIL
           LOAD      0,&L                F0 = DET
 Z&SYSNDX  DS        0H
 &STACK    SETA      &OLD
           MEND
           MACRO
 &LABEL    TSFACT    &N,&A,&IA,&JA,&X,&Y,&I,&J,&L
#include "kernnum/global.inc"
           LCLA      &OLD,&A1,&A1K1,&AK1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &A1       SETA      &OLD
 &A1K1     SETA      &A1+1*4
 &AK1      SETA      &A1K1+1*4
 &STACK    SETA      &AK1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     13,'NO SPACE FOR MACRO TSFACT'
           MEXIT
 .ALPHA    ANOP
           ST        &A,&A1.(15)         &A1 = (A(1,1))
           ST        &A,&AK1.(15)        &AK1 = (A(K,1))
           LR        &J,&A
           AR        &J,&JA              &J = (A(1,K+1))
           ST        &J,&A1K1.(15)       &A1K1 = (A(1,K+1))
 *         IFAIL  =  0
           LA        &J,0                &J = 0
           ST        &J,IFL&SYSNDX       IFAIL = 0
 *         DO  20    K  =  1, N
           LA        &I,0                &I = 0 = K-1
 A&SYSNDX  DS        0H
 *                                       &I = K-1
 *                                       &A = (A(K,K))
 *                                       &A1K1 = (A(1,K+1))
 *                                       &AK1  = (A(K,1))
 *         IF(A(K,K) .LE. 0.)     GOTO 90
 *         A(K,K)  =  ONE / A(K,K)
           LOAD      0,&A                F0 = A(K,K)
           CE        0,=E'0.'
           BC        LE,E&SYSNDX         IF(A(K,K) .LE. 0.) GOTO 90
           RCPRCL    &A                  A(K,K)  =  1 / A(K,K)
 * 14      IF(K .EQ. N)  GOTO 21
           LR        &J,&I               &J = K-1
           A         &J,=F'1'            &J = K
           CR        &J,&N
           BC        EQ,F&SYSNDX         IF(K .EQ. N)
 *         CALL VSCL(N-K,A(K,K),A(K+1,K),IA,A(K,K+1),JA)
 *         CALL MMNA(N-K,K,A(K+1,1),IA,JA,A(1,K+1),IA,A(K+1,K+1),IA)
 D&SYSNDX  DS        0H
           LOAD      4,&A                F4 = A(K,K)
           LR        &L,&I               &L = K-1
           A         &L,=F'1'            &L = K
           LR        &I,&N
           SR        &I,&L               &I = N-K
           LR        &X,&A               &X = (A(K,K))
           LR        &Y,&X               &Y = (A(K,K))
           AR        &Y,&JA              &Y = (A(K,K+1))
           AR        &X,&IA              &X = (A(K+1,K))
           TVSCL     &I,&X,&IA,&Y,&JA
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
           B         A&SYSNDX            REPEAT MAIN LOOP
 IFL&SYSNDX   DS     1F                  IFAIL
 * 90      IFAIL  =  -1
 E&SYSNDX  DS        0H
           L         &I,=F'-1'           &I = -1
           B         Z&SYSNDX            EXIT
 * 21      RETURN
 F&SYSNDX  L         &A,&A1.(15)         &A = (A)
           L         &I,IFL&SYSNDX       &I = IFAIL
 Z&SYSNDX  DS        0H
 &STACK    SETA      &OLD
           MEND

#endif
