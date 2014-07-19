*
* $Id: add.inc,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: add.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_ADD_INC
#define CERNLIB_KERNNUM_ADD_INC
*
*
* add.inc
*
           MACRO
 &LABEL    ADD       &F,&X,&Y,&Z,&JX=0,&JY=0,&JZ=0
#include "kernnum/global.inc"
 &LABEL    LOAD      0,&X,JX=&JX
           AIF       ('&T' EQ 'R').RADD
           AIF       ('&T' EQ 'D').DADD
           AIF       ('&T' EQ 'C').CADD
           MEXIT
 .CADD     ANOP
           &F&W      2,&D.(&Y,&JY)
           ST&W      2,&D.(&Z,&JZ)
 .RADD     ANOP
 .DADD     ANOP
           &F&W      0,0(&Y,&JY)
           ST&W      0,0(&Z,&JZ)
           MEND
           MACRO
 &LABEL    TVADD     &F,&N,&X,&JX,&Y,&JY,&Z,&JZ
#include "kernnum/global.inc"
           PUSH      &X                  STACK (X(1))
           PUSH      &Y                  STACK (Y(1))
           PUSH      &Z                  STACK (Z(1))
 L&SYSNDX  LOOPONE   &N                  FOR J = 1, N
           ADD       &F,&X,&Y,&Z
           AR        &X,&JX              &X = (X(J+1))
           AR        &Y,&JY              &Y = (Y(J+1))
           AR        &Z,&JZ              &Z = (Z(J+1))
           LEND      &N,L&SYSNDX
           POP       &Z                  RESTORE &Z = (Z(1))
           POP       &Y                          &Y = (Y(1))
           POP       &X                          &X = (X(1))
           MEND
           MACRO
 &LABEL    VADD      &F,&N,&X,&JX,&Y,&JY,&Z,&JZ,&JX2,&JY2,&JZ2,&J
 &LABEL    FIRST     &J,&N,LOOP=L&SYSNDX
           ADD       &F,&X,&Y,&Z
           AR        &X,&JX                        (X) = (X) + JX
           AR        &Y,&JY                        (Y) = (Y) + JY
           AR        &Z,&JZ                        (Z) = (Z) + JZ
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           ADD       &F,&X,&Y,&Z
           ADD       &F,&X,&Y,&Z,JX=&JX,JY=&JY,JZ=&JZ
           AR        &X,&JX2                       (X) = (X) + 2*JX
           AR        &Y,&JY2                       (Y) = (Y) + 2*JY
           AR        &Z,&JZ2                       (Z) = (Z) + 2*JZ
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVADD2    &F,&N,&X,&JX,&Y,&JY,&Z,&JZ,&JX2,&JY2,&JZ2,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X1,&Y1,&Z1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &X1       SETA      &OLD
 &Y1       SETA      &X1+1*4
 &Z1       SETA      &Y1+1*4
 &STACK    SETA      &Z1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     12,'NO SPACE FOR MACRO TVADD2'
           MEXIT
 .ALPHA    ANOP
           ST        &X,&X1.(15)         X1 = (X)
           ST        &Y,&Y1.(15)         Y1 = (Y)
           ST        &Z,&Z1.(15)         Z1 = (Z)
           VADD      &F,&N,&X,&JX,&Y,&JY,&Z,&JZ,&JX2,&JY2,&JZ2,&J
           L         &X,&X1.(15)         &X = (X)
           L         &Y,&Y1.(15)         &Y = (Y)
           L         &Z,&Z1.(15)         &Z = (Z)
 &STACK    SETA      &OLD
           MEND
           MACRO
 &LABEL    TMADD     &F,&M,&N,&X,&IX,&JX,&Y,&IY,&JY,&Z,&IZ,&JZ
#include "kernnum/global.inc"
 &LABEL    DS        0H
 A&SYSNDX  PUSH      &M
           PUSH      &X
           PUSH      &Y
           PUSH      &Z
           LA        0,1
           NR        0,&M                R0 = MOD(M,2)
           BZ        B&SYSNDX            IF M IS EVEN
           TVADD     &F,&N,&X,&JX,&Y,&JY,&Z,&JZ
           S         &M,=F'1'            M  =  M-1
           BZ        E&SYSNDX            IF M = 1
           AR        &X,&IX              &X = (X(2,1))
           AR        &Y,&IY              &Y = (Y(2,1))
           AR        &Z,&IZ              &Z = (Z(2,1))
 B&SYSNDX  LA        0,1                 R0 = NSTEP = 1
           LR        1,&N                R1 = N
 C&SYSNDX  PUSH      &X                  &X = (X(I,1))
           PUSH      &Y                  &Y = (Y(I,1))
           PUSH      &Z                  &Z = (Z(I,1))
           LA        &N,1                &N = J = 1
 D&SYSNDX  ADD       &F,&X,&Y,&Z
           ADD       &F,&X,&Y,&Z,JX=&IX,JY=&IY,JZ=&IZ
           AR        &X,&JX              &X = (X(I,J+1))
           AR        &Y,&JY              &Y = (Y(I,J+1))
           AR        &Z,&JZ              &Z = (Z(I,J+1))
           BXLE      &N,0,D&SYSNDX
           POP       &Z                  &Z = (Z(I,1))
           POP       &Y                  &Y = (Y(I,1))
           POP       &X                  &X = (X(I,1))
           AR        &Z,&IZ
           AR        &Y,&IY
           AR        &X,&IX
           AR        &Z,&IZ              &Z = (Z(I+2,1))
           AR        &Y,&IY              &Y = (Y(I+2,1))
           AR        &X,&IX              &X = (X(I+2,1))
           S         &M,=F'2'            M  = M-2
           BNZ       C&SYSNDX
 E&SYSNDX  LR        &N,1                &N = N
           POP       &Z                  &Z = (Z(1,1))
           POP       &Y                  &Y = (Y(1,1))
           POP       &X                  &X = (X(1,1))
           POP       &M                  &M = M
           MEND

#endif
