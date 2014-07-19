*
* $Id: set.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: set.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_SET_INC
#define CERNLIB_KERNNUM_SET_INC
*
*
* set.inc
*
           MACRO
 &LABEL    VSET      &N,&Z,&JZ,&JZ2,&J
 &LABEL    FIRST     &J,&N,LOOP=L&SYSNDX
           STORE     0,&Z
           AR        &Z,&JZ                        (Z) = (Z) + JZ
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           STORE     0,&Z
           STORE     0,&Z,JX=&JZ
           AR        &Z,&JZ2                       (Z) = (Z) + 2*JZ
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVSET2    &N,&Z,&JZ,&JZ2,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&Z1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &Z1       SETA      &OLD
 &STACK    SETA      &Z1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     14,'NO WORK SPACE FOR MACRO TVSET2'
           MEXIT
 .ALPHA    ANOP
           ST        &Z,&Z1.(15)                   Z1 = (Z)
           VSET      &N,&Z,&JZ,&JZ2,&J
           L         &Z,&Z1.(15)                   &Z = (Z)
 &STACK    SETA      &OLD
           MEND
           MACRO
 &LABEL    MSET      &M,&N,&Z,&IZ,&JZ,&JZ2,&I,&J
 &LABEL    DS        0H
 L&SYSNDX  FORONE    &I
           TVSET2    &N,&Z,&JZ,&JZ2,&J
           AR        &Z,&IZ              &Z = (Z(I+1,1))
           ENDFOR    &I,&M,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TMSET2    &M,&N,&Z,&IZ,&JZ,&JZ2,&I,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&Z11
 &OLD      SETA      &STACK
 &Z11      SETA      &OLD
 &STACK    SETA      &Z11+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     14,'NO WORK SPACE FOR MACRO TMSET2'
           MEXIT
 .ALPHA    ANOP
 &LABEL    ST        &Z,&Z11.(15)        Z11 = (Z(1,1))
           CR        &M,&N
           BC        GT,A&SYSNDX
           LA        &M-1,1              PROCESS 1 ROW AT A TIME
           LA        &N-1,2              PROCESS 2 ELEMENTS AT A TIME
           LR        &JZ2,&JZ
           AR        &JZ2,&JZ            &JZ2 = 2*JZ
           MSET      &M,&N,&Z,&IZ,&JZ,&JZ2,&I,&J
           B         B&SYSNDX
 A&SYSNDX  LA        &N-1,1              PROCESS 1 COLUMN AT A TIME
           LA        &M-1,2              PROCESS 2 ELEMENTS AT A TIME
           LR        &JZ2,&IZ
           AR        &JZ2,&IZ            &JZ2 = 2*IZ
           MSET      &N,&M,&Z,&JZ,&IZ,&JZ2,&J,&I
 B&SYSNDX  L         &Z,&Z11.(15)        &Z = (Z(1,1))
 &STACK    SETA      &OLD
           MEND

#endif
