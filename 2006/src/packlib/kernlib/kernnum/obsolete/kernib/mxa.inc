*
* $Id: mxa.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: mxa.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_MXA_INC
#define CERNLIB_KERNNUM_MXA_INC
*
*
* mxa.inc
*
 *         F4 = S
 *         &L = location(S)
 *         &J = location(X)
 *
           MACRO
 &LABEL    MXA       &X,&L,&J
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RMXA
           AIF       ('&T' EQ 'D').DMXA
           AIF       ('&T' EQ 'C').CMXA
           MNOTE     13,'NO TYPE DEFINED FOR MACRO MXA'
           MEXIT
 .CMXA     LD        0,=D'0'             F0 = 0.
           L&W       0,&D.(&X)           F0 = X''
           LPDR      0,0                 F0 = ABS(X'')
           CDR       0,4
           BC        LE,C&SYSNDX         IF F0 .LE. F4
           LDR       4,0                 F4 = F0
           LR        &L,&J               &L = &J
 C&SYSNDX  DS        0H
 .RMXA     LD        0,=D'0'             F0 = 0.
 .DMXA     L&W       0,0(&X)             F0 = X' = X
           LPDR      0,0                 F0 = ABS(X)
           CDR       0,4
           BC        LE,A&SYSNDX         IF F0 .LE. F4
           LDR       4,0                 F4 = F0
           LR        &L,&J               &L = &J
 A&SYSNDX  DS        0H
           MEND
           MACRO
 &LABEL    VMXA      &N,&X,&JX,&L,&J
 &LABEL    DS        0H
           LD        4,=D'0'             F4 = 0.
           LA        &L,1                &L = 1
 L&SYSNDX  FORONE    &J
           MXA       &X,&L,&J
           AR        &X,&JX                        (X) = (X) + JX
           ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVMXA1    &N,&X,&JX,&L,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &X1       SETA      &OLD
 &STACK    SETA      &X1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     14,'NO SPACE FOR MACRO TVMXA1'
           MEXIT
 .ALPHA    ANOP
           ST        &X,&X1.(15)         X1 = (X)
           VMXA      &N,&X,&JX,&L,&J
           L         &X,&X1.(15)         &X = (X)
 &STACK    SETA      &OLD
           MEND
           MACRO
 &LABEL    MMXA      &M,&N,&X,&IX,&JX,&L,&I,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&AMAX
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &AMAX     SETA      &OLD                CUMULATIVE MAXIMUM VALUE
 &STACK    SETA      &AMAX+2*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     14,'NO SPACE FOR MACRO MMXA'
           MEXIT
 .ALPHA    ANOP
           LD        4,=D'0'
           STD       4,&AMAX             &AMAX = 0.
 L&SYSNDX  FORONE    &I
           TVMXA1    &N,&X,&JX,&L,&J
           AR        &X,&IX                        &X = (X(I+1,1))
           CD        4,&AMAX
           BC        LE,A&SYSNDX
           STD       4,&AMAX             &AMAX = AMAX1(F4,&AMAX)
 A&SYSNDX  ENDFOR    &I,&M,LOOP=L&SYSNDX
           LD        0,&AMAX             F0 = &AMAX
 &STACK    SETA      &OLD
           MEND
           MACRO
 &LABEL    TMMXA1    &M,&N,&X,&IX,&JX,&L,&I,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X11
 &OLD      SETA      &STACK
 &X11      SETA      &OLD
 &STACK    SETA      &X11+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     14,'NO SPACE FOR MACRO TMMXA1'
           MEXIT
 .ALPHA    ANOP
 &LABEL    ST        &X,&X11.(15)        X11 = (X)
           CR        &M,&N
           BC        GT,A&SYSNDX
           LA        &M-1,1              PROCESS 1 ROW AT A TIME
           LA        &N-1,1              PROCESS 1 ELEMENT AT A TIME
           MMAX      &M,&N,&X,&IX,&JX,&L,&I,&J
           B         B&SYSNDX
 A&SYSNDX  LA        &N-1,1              PROCESS 1 COLUMN AT A TIME
           LA        &M-1,1              PROCESS 1 ELEMENT AT A TIME
           MMAX      &N,&M,&X,&JX,&IX,&L,&J,&I
 B&SYSNDX  L         &X,&X11.(15)        &X = (X)
 &STACK    SETA      &OLD
           MEND

#endif
