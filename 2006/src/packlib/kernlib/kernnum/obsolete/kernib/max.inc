*
* $Id: max.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: max.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_MAX_INC
#define CERNLIB_KERNNUM_MAX_INC
*
*
* max.inc
*
           MACRO
 &LABEL    MAX       &F,&X,&L,&J         &F = GE (MAX) OR LE (MIN)
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').ALPHA
           AIF       ('&T' EQ 'D').ALPHA
           MNOTE     13,'NO TYPE DEFINED FOR MACRO MAX'
           MEXIT
 .ALPHA    C&W       0,0(&X)
           BC        &F,A&SYSNDX         IF(F0 .GE/LE. X)
           AIF       ('&T EQ 'D').BETA
           LD        0,=D'0'
 .BETA     L&W       0,0(&X)             F0 = X
           LR        &L,&J               &L = &J
 A&SYSNDX  DS        0H
           MEND
           MACRO
 &LABEL    VMAX      &N,&X,&JX,&L,&J
 &LABEL    DS        0H
           LD        0,=D'0'             F0 = 0.
           LA        &L,1                &L = 1
 L&SYSNDX  FORONE    &J
           MAX       &X,&L,&J
           AR        &X,&JX                        (X) = (X) + JX
           ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVMAX1    &N,&X,&JX,&L,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &X1       SETA      &OLD
 &STACK    SETA      &X1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     14,'NO SPACE FOR MACRO TVMAX1'
           MEXIT
 .ALPHA    ANOP
           ST        &X,&X1              X1 = (X)
           VMAX      &N,&X,&JX,&L,&J
           L         &X,&X1              &X = (X)
 &STACK    SETA      &OLD
           MEND

#endif
