*
* $Id: cpy.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: cpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_CPY_INC
#define CERNLIB_KERNNUM_CPY_INC
*
*
* cpy.inc
*
           MACRO
 &LABEL    CPY       &X,&Z,&JX=0,&JZ=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RCPY
           AIF       ('&T' EQ 'D').DCPY
           AIF       ('&T' EQ 'C').CCPY
           MEXIT
 .CCPY     AIF       (&CC NE 0).CCPYC
           LD        0,0(&X,&JX)
           STD       0,0(&Z,&JZ)
           MEXIT
 .CCPYC    L&W       0,0(&X,&JX)
           ST&W      0,0(&Z,&JZ)
           L&W       0,&D.(&X,&JX)
           LC&W.R    0,0                 INVERT SIGN OF IMAGINARY PART
           ST&W      0,&D.(&Z,&JZ)
           MEXIT
 .RCPY     ANOP
 .DCPY     ANOP
           L&W       0,0(&X,&JX)
           ST&W      0,0(&Z,&JZ)
           MEND
           MACRO
 &LABEL    TVCPY     &N,&X,&JX,&Z,&JZ
#include "kernnum/global.inc"
           PUSH      &X                  STACK (X(1))
           PUSH      &Z                  STACK (Z(1))
 L&SYSNDX  LOOPONE   &N                  FOR J = 1, N
           CPY       &X,&Z
           AR        &X,&JX              &X = (X(J+1))
           AR        &Z,&JZ              &Z = (Z(J+1))
           LEND      &N,L&SYSNDX
           POP       &Z                  RESTORE &Z = (Z(1))
           POP       &X                          &X = (X(1))
           MEND
           MACRO
 &LABEL    VCPY      &N,&X,&JX,&Z,&JZ,&JX2,&JZ2,&J
 &LABEL    FIRST     &J,&N,LOOP=L&SYSNDX
           CPY       &X,&Z
           AR        &X,&JX                        (X) = (X) + JX
           AR        &Z,&JZ                        (Z) = (Z) + JZ
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           CPY       &X,&Z
           CPY       &X,&Z,JX=&JX,JZ=&JZ
           AR        &X,&JX2                       (X) = (X) + 2*JX
           AR        &Z,&JZ2                       (Z) = (Z) + 2*JZ
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVCPY2    &N,&X,&JX,&Z,&JZ,&JX2,&JZ2,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X1,&Z1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &X1       SETA      &OLD
 &Z1       SETA      &X1+1*4
 &STACK    SETA      &Z1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     12,'INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           ST        &X,&X1.(15)         X1 = (X)
           ST        &Z,&Z1.(15)         Z1 = (Z)
           VCPY      &N,&X,&JX,&Z,&JZ,&JX2,&JZ2,&J
           L         &X,&X1.(15)         &X = (X)
           L         &Z,&Z1.(15)         &Z = (Z)
 &STACK    SETA      &OLD
           MEND
           MACRO
 &LABEL    TMCPY     &M,&N,&X,&IX,&JX,&Z,&IZ,&JZ
#include "kernnum/global.inc"
 &LABEL    DS        0H
 A&SYSNDX  PUSH      &M
           PUSH      &X
           PUSH      &Z
           LA        0,1
           NR        0,&M                R0 = MOD(M,2)
           BZ        C&SYSNDX            IF M IS EVEN
           TVCPY     &N,&X,&JX,&Z,&JZ
           S         &M,=F'1'            M  =  M-1
           BZ        F&SYSNDX            IF M = 1
           AR        &X,&IX              &X = (X(2,1))
           AR        &Z,&IZ              &Z = (Z(2,1))
 C&SYSNDX  LA        0,1                 R0 = NSTEP = 1
           LR        1,&N                R1 = N
 D&SYSNDX  PUSH      &X                  &X = (X(I,1))
           PUSH      &Z                  &Z = (Z(I,1))
           LA        &N,1                &N = J = 1
 E&SYSNDX  CPY       &X,&Z
           CPY       &X,&Z,JX=&IX,JZ=&IZ
           AR        &X,&JX              &X = (X(I,J+1))
           AR        &Z,&JZ              &Z = (Z(I,J+1))
           BXLE      &N,0,E&SYSNDX
           POP       &Z                  &Z = (Z(I,1))
           POP       &X                  &X = (X(I,1))
           AR        &Z,&IZ
           AR        &X,&IX
           AR        &Z,&IZ              &Z = (Z(I+2,1))
           AR        &X,&IX              &X = (X(I+2,1))
           S         &M,=F'2'            M  = M-2
           BNZ       D&SYSNDX
 F&SYSNDX  LR        &N,1                &N = N
           POP       &Z                  &Z = (Z(1,1))
           POP       &X                  &X = (X(1,1))
           POP       &M                  &M = M
           MEND

#endif
