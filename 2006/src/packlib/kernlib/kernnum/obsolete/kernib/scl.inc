*
* $Id: scl.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: scl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_SCL_INC
#define CERNLIB_KERNNUM_SCL_INC
*
*
* scl.inc
*
           MACRO
 &LABEL    SCL       &X,&Z,&JX=0,&JZ=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RSCL
           AIF       ('&T' EQ 'D').DSCL
           AIF       ('&T' EQ 'C').CSCL
           MEXIT
 .RSCL     ANOP
 .DSCL     ANOP
           LDR       0,4                 F0=S
           M&W       0,0(&X,&JX)
           ST&W      0,0(&Z,&JZ)
           MEXIT
 .CSCL     ANOP
           SDR       0,0
           SDR       2,2
           L&W       0,0(&X,&JX)         F0=X'
           L&W       2,&D.(&X,&JX)                 F2=X''
           M&W.R     0,4                 F0 = S'*X'
           M&W.R     2,6                 F2 = S''*X''
           S&W.R     0,2                 F0 = S'*X' - S''*X''
           AIF       ('T' EQ 'W').ALPHA
           SDR       2,2
 .ALPHA    L&W       2,0(&X,&JX)         F2=X'
           ST&W      0,0(&Z,&JZ)         Z' = S'*X' - S''*X''
           AIF       ('T' EQ 'W').BETA
           SDR       0,0
 .BETA     L&W       0,&D.(&X,&JX)       F0=X''
           M&W.R     0,4                 F0 = S'*X''
           M&W.R     2,6                 F2 = S''*X'
           A&W.R     0,2                 F0 = S'*X'' + S''*X'
           ST&W      0,&D.(&Z,&JZ)       Z''= S'*X'' + S''*X'
           MEND
           MACRO
 &LABEL    TVSCL     &N,&X,&JX,&Z,&JZ
#include "kernnum/global.inc"
 &LABEL    PUSH      &X
           PUSH      &Z
 L&SYSNDX  LOOPONE   &N
           SCL       &X,&Z
           AR        &X,&JX              &X = (X(J+1))
           AR        &Z,&JZ              &Z = (Z(J+1))
           LEND      &N,L&SYSNDX
           POP       &Z                  &Z = (Z(1))
           POP       &X                  &X = (X(1))
           MEND
           MACRO
 &LABEL    VSCL      &N,&X,&JX,&Z,&JZ,&JX2,&JZ2,&J
 &LABEL    FIRST     &J,&N,LOOP=L&SYSNDX
           SCL       &X,&Z
           AR        &X,&JX                        (X) = (X) + JX
           AR        &Z,&JZ                        (Z) = (Z) + JZ
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           SCL       &X,&Z
           SCL       &X,&Z,JX=&JX,JZ=&JZ
           AR        &X,&JX2                       (X) = (X) + 2*JX
           AR        &Z,&JZ2                       (Z) = (Z) + 2*JZ
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVSCL2    &N,&X,&JX,&Z,&JZ,&JX2,&JZ2,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X1,&Z1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &X1       SETA      &OLD
 &Z1       SETA      &X1+1*4
 &STACK    SETA      &Z1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     255,'INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           ST        &X,&X1.(15)         X1 = (X)
           ST        &Z,&Z1.(15)         Z1 = (Z)
           VSCL      &N,&X,&JX,&Z,&JZ,&JX2,&JZ2,&J
           L         &X,&X1.(15)         &X = (X)
           L         &Z,&Z1.(15)         &Z = (Z)
 &STACK    SETA      &OLD
           MEND
           MACRO
 &LABEL    TMSCL     &M,&N,&X,&IX,&JX,&Z,&IZ,&JZ
#include "kernnum/global.inc"
 &LABEL    DS        0H
 A&SYSNDX  PUSH      &M
           PUSH      &X
           PUSH      &Z
           LR        1,&N                R1 = N
           LA        0,1
           NR        0,&M                R0 = MOD(M,2)
           BZ        C&SYSNDX            IF M IS EVEN
           TVSCL     &N,&X,&JX,&Z,&JZ
           SR        &M,0                M  =  M-1
           BZ        F&SYSNDX            IF M = 1
           AR        &X,&IX              &X = (X(2,1))
           AR        &Z,&IZ              &Z = (Z(2,1))
 C&SYSNDX  LA        0,1                 R0 = NSTEP = 1
 D&SYSNDX  PUSH      &X                  &X = (X(I,1))
           PUSH      &Z                  &Z = (Z(I,1))
           LA        &N,1                &N = J = 1
 E&SYSNDX  SCL       &X,&Z
           SCL       &X,&Z,JX=&IX,JZ=&IZ
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
           MACRO
 &LABEL    RSCL      &M,&N,&DD,&ID,&X,&IX,&JX,&Z,&IZ,&JZ
#include "kernnum/global.inc"
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &IX1      SETA      &OLD                SAVE OF &IX
 &IZ1      SETA      &IX1+1*4            SAVE OF &IZ
 &STACK    SETA      &IZ1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     12,'INSUFFICIENT WORK SPACE FOR RSCL'
           MEXIT
 .ALPHA    ANOP
           ST        &IX,&IX1.(15)       SAVE &IX
           ST        &IZ,&IZ1.(15)       SAVE &IZ
           LR        &IX,&JX             &IX= JX
           LR        &IZ,&JZ             &IZ= JZ
           AR        &IX,&JX             &IX= 2*JX
           AR        &IZ,&JZ             &IZ= 2*JZ
 A&SYSNDX  LOAD      4,&DD               F4 = D(I)
           PUSH      &DD                 SAVE (D(I))
           PUSH      &X                  SAVE (X(I,1))
           PUSH      &Z                  SAVE (Z(I,1))
           VSCL      &N,&X,&JX,&Z,&JZ,&IX,&IZ,&DD
           POP       &Z                  &Z = (Z(I,1))
           POP       &X                  &X = (X(I,1))
           POP       &DD                 &DD= (D(I))
           A         &Z,&IZ1.(15)        &Z = (Z(I+1,1))
           A         &X,&IX1.(15)        &X = (X(I+1,1))
           AR        &DD,&ID             &DD= (D(I+1))
           S         &M,=F'1'            &M = M-I
           BNZ       A&SYSNDX            IF(I .LT. M) REPEAT
           L         &IX,&IX1.(15)       RESTORE &IX
           L         &IZ,&IZ1.(15)       RESTORE &IZ
 &STACK    SETA      &OLD
           MEND

#endif
