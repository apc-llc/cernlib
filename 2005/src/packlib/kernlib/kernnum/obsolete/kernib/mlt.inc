*
* $Id: mlt.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: mlt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_MLT_INC
#define CERNLIB_KERNNUM_MLT_INC
*
*
* mlt.inc
*
           MACRO
 &LABEL    TMMLT     &M,&N,&X,&IX,&JX,&Y,&JY,&LY,&Z,&IZ,&LZ R0=(T),R1=K
#include "kernnum/global.inc"
 &LABEL    CR        &Z,&X
           BE        C&SYSNDX            IF (Z) = (X)
           CR        &Z,&Y
           BE        D&SYSNDX            IF (Z) = (Y)
           CR        &JX,&JY
           BE        B&SYSNDX            IF JX = JY  GOTO B
 A&SYSNDX  ZISXY     &M,&N,&X,&IX,&JX,&Y,&JY,&LY,&Z,&IZ,&LZ  R1=K
           B         Z&SYSNDX
 *         IF (Y) .NE. (X)  GOTO A
 *         IF LY  .NE. IX   GOTO A
 *         IF K   .NE. M    GOTO A
 B&SYSNDX  CR        &Y,&X
           BNE       A&SYSNDX            IF (Y) .NE. (X)  GOTO A
           CR        &LY,&IX
           BNE       A&SYSNDX            IF LY .NE. IX  GOTO A
           CR        1,&M
           BNE       A&SYSNDX            IF K .NE. M  GOTO A
           ZISXXT    &M,&N,&X,&IX,&JX,&Z,&IZ,&LZ,&Y
           B         Z&SYSNDX
 C&SYSNDX  LR        &Z,0                &Z = (T)
           CR        &X,&Y
           BE        E&SYSNDX            IF (X) = (Y)  GOTO E
           LR        &IZ,1               &IZ = K
           LA        &LZ,&LENGTH         &LZ = STEP SIZE OF ARRAY T
           XISXY     &M,&N,&IZ,&X,&IX,&JX,&Y,&JY,&LY,&Z,&LZ
           B         Z&SYSNDX
 D&SYSNDX  LR        &Z,0                &Z = (T)
           LR        &IZ,1               &IZ = K
           LA        &LZ,&LENGTH         &LZ = STEP SIZE OF ARRAY T
           YISXY     &M,&N,&IZ,&X,&IX,&JX,&Y,&JY,&LY,&Z,&LZ
           B         Z&SYSNDX
 E&SYSNDX  LA        &LZ,&LENGTH         &LZ = STEP SIZE FOR ARRAY T
           CR        &IZ,&JX
           BE        F&SYSNDX            IF Z=X' GOTO F
           XISXXT    &M,&N,&X,&IX,&JX,&Z,&LZ,&JY,&LY
           B         Z&SYSNDX
 F&SYSNDX  XTISXXT   &M,&N,&X,&IX,&JX,&Z,&LZ,&JY,&LY
 Z&SYSNDX  DS        0H
           MEND
           MACRO
 &LABEL    ZISXY     &M,&N,&X,&IX,&JX,&Y,&JY,&LY,&Z,&IZ,&LZ  R1=K
#include "kernnum/global.inc"
 &LABEL    PUSH      1                   STACK K
           PUSH      &Y                  STACK (Y(1,1))
           PUSH      &Z                  STACK (Z(1,1))
 A&SYSNDX  PUSH      1                   SAVE K-I+1
           TXMPY     MMPY,&M,&N,&X,&IX,&JX,&Y,&JY,&Z,&IZ
           AR        &Y,&LY              &Y = (Y(1,I+1))
           AR        &Z,&LZ              &Z = (Z(1,I+1))
           POP       1                   R1 = K-I+1
           S         1,=F'1'             R1 = K-I
           BNZ       A&SYSNDX
           POP       &Z                  &Z = (Z(1,1))
           POP       &Y                  &Y = (Y(1,1))
           POP       1                   R1 = K
           MEND
           MACRO
 &LABEL    ZISXXT    &M,&N,&X,&IX,&JX,&Z,&IZ,&LZ,&Y  (&Y IS TEMP)
#include "kernnum/global.inc"
 &LABEL    PUSH      &X                  STACK (X(1,1))
           PUSH      &M                  STACK M
           PUSH      &Z                  STACK (Z(1,1))
 A&SYSNDX  LR        &Y,&X               &Y = (X(I,1))
           TXMPY     MMPY,&M,&N,&X,&IX,&JX,&Y,&JX,&Z,&IZ
           AR        &Z,&IZ              &Z = (Z(I+1,I))
           AR        &X,&IX              &X = (X(I+1,1))
           AR        &Z,&LZ              &Z = (Z(I+1,I+1))
           S         &M,=F'1'            M  = M-I
           BNZ       A&SYSNDX
           POP       &Z                  &Z = (Z(1,1))
           POP       &M                  &M = M
           TMUTL     &M,&Z,&LZ,&IZ,&X    (&X IS TEMP)
           POP       &X                  &X = (X(1,1))
           MEND
           MACRO
 &LABEL    XISXY     &M,&N,&K,&X,&IX,&JX,&Y,&JY,&LY,&TT,&JT
#include "kernnum/global.inc"
           LCLA      &OLDCC              SAVE OF CURRENT &CC
 &LABEL    PUSH      &M                  STACK M
           PUSH      &X                  STACK (X(1,1))
 &OLDCC    SETA      &CC
           AIF       (&CC EQ 0).ALPHA
 &CC       SETA      3-&CC               1 FOR 2 AND VICE VERSA
 .ALPHA    ANOP
 A&SYSNDX  TXMPY     MMPY,&K,&N,&Y,&LY,&JY,&X,&JX,&TT,&JT
 &CC       SETA      0                   COPY WITHOUT CONJUGATION
           TVCPY     &K,&TT,&JT,&X,&JX
 &CC       SETA      &OLDCC              RESTORE &CC
           AR        &X,&IX              &X = (X(I+1,1))
           S         &M,=F'1'            &M = M-I
           BNZ       A&SYSNDX
           POP       &X                  &X = (X(1,1))
           POP       &M                  &M = M
           MEND
           MACRO
 &LABEL    YISXY     &M,&N,&K,&X,&IX,&JX,&Y,&JY,&LY,&TT,&JT
#include "kernnum/global.inc"
           LCLA      &OLDCC              SAVE OF CURRENT &CC
 &LABEL    PUSH      &K                  STACK K
           PUSH      &Y                  STACK (Y(1,1))
 A&SYSNDX  TXMPY     MMPY,&M,&N,&X,&IX,&JX,&Y,&JY,&TT,&JT
 &OLDCC    SETA      &CC
 &CC       SETA      0                   COPY WITHOUT CONJUGATION
           TVCPY     &M,&TT,&JT,&Y,&JY
 &CC       SETA      &OLDCC              RESTORE &CC
           AR        &Y,&LY              &Y = (Y(1,I+1))
           S         &K,=F'1'            &K = K-I
           BNZ       A&SYSNDX
           POP       &Y                  &Y = (Y(1,1))
           POP       &K                  &K = K
           MEND
           MACRO
 &LABEL    XISXXT    &M,&N,&X,&IX,&JX,&TT,&JT,&Y,&Z  (&Y, &Z ARE TEMP)
#include "kernnum/global.inc"
 &LABEL    PUSH      &M                  STACK M
           PUSH      &X                  STACK (X(1,1))
           LR        &Z,&X               &Z = (X(1,1))
 A&SYSNDX  LR        &Y,&X               &Y = (X(I,1))
           TXMPY     MMPY,&M,&N,&X,&IX,&JX,&Y,&JX,&TT,&JT
 *                                       COPY WITH CONJUGATION
           TVCPY     &M,&TT,&JT,&Z,&JX
           AR        &Z,&IX              &Z = (X(I+1,I))
           AR        &X,&IX              &X = (X(I+1,1))
           AR        &Z,&JX              &Z = (X(I+1,I+1))
           S         &M,=F'1'            &M = M-I
           BNZ       A&SYSNDX
           POP       &X                  &X = (X(1,1))
           POP       &M                  &M = M
           TMUTL     &M,&X,&IX,&JX,&Y
           MEND
           MACRO
 &LABEL    XTISXXT   &M,&N,&X,&IX,&JX,&TT,&JT,&Y,&Z  (&Y, &Z ARE TEMP)
#include "kernnum/global.inc"
           LCLA      &OLDCC              SAVE OF CURRENT &CC
 &LABEL    PUSH      &M                  STACK M
           PUSH      &X                  STACK (X(1,1))
           LR        &Z,&X               &Z = (X(1,1))
 A&SYSNDX  LR        &Y,&X               &Y = (X(I,1))
           TXMPY     MMPY,&M,&N,&X,&IX,&JX,&Y,&JX,&TT,&JT
 &OLDCC    SETA      &CC
 &CC       SETA      0                   COPY WITHOUT CONJUGATION
           TVCPY     &M,&TT,&JT,&Z,&JX
 &CC       SETA      &OLDCC              RESTORE &CC
           AR        &Z,&IX              &Z = (X(I+1,I))
           AR        &X,&IX              &X = (X(I+1,1))
           AR        &Z,&JX              &Z = (X(I+1,I+1))
           S         &M,=F'1'            &M = M-I
           BNZ       A&SYSNDX
           POP       &X                  &X = (X(1,1))
           POP       &M                  &M = M
           TMUTL     &M,&X,&IX,&JX,&Y
           MEND

#endif
