*
* $Id: sqr.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: sqr.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_SQR_INC
#define CERNLIB_KERNNUM_SQR_INC
*
*
* sqr.inc
*
           MACRO
 &LABEL    SQR       &X,&JX=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RSQR
           AIF       ('&T' EQ 'D').DSQR
           AIF       ('&T' EQ 'C').CSQR
           MEXIT
 .CSQR     ANOP
           SDR       4,4                 F4 = 0
           L&W       4,&D.(&X,&JX)       F4 = X''
           M&W.R     4,4                 F4 = X''**2
           SDR       0,4                 F0 = S'-X''**2
           L&W       4,0(&X,&JX)         F4 = X'
           M&W       4,&D.(&X,&JX)       F4 = X'X''
           ADR       2,4                 F2 = 0.5S''+X'X''
 .RSQR     ANOP
           SDR       4,4                 F4 = 0
 .DSQR     ANOP
           L&W       4,0(&X,&JX)         F4 = X'
           M&W.R     4,4                 F4 = X'**2
           ADR       0,4                 F0 = S'+X'**2
           MEND
           MACRO
 &LABEL    VSQR      &N,&X,&JX,&JX2,&J
#include "kernnum/global.inc"
 &LABEL    CLEAR     0                   F0 = 0
           FIRST     &J,&N,LOOP=L&SYSNDX
           SQR       &X
           AR        &X,&JX              (X) = (X) + JX
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           SQR       &X
           SQR       &X,JX=&JX
           AR        &X,&JX2             (X) = (X) + 2*JX
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           AIF       ('&T' EQ 'C').ALPHA
           AIF       ('&T' EQ 'W').ALPHA
           AGO       .BETA
 .ALPHA    ADR       2,2                 S' = 2*S'
 .BETA     ANOP
           AIF       ('&T' EQ 'R').RROUND
           AIF       ('&T' EQ 'C').CROUND
           MEXIT
 .CROUND   LRER      2,2
 .RROUND   LRER      0,0
           MEND
           MACRO
 &LABEL    TVSQR2    &N,&X,&JX,&JX2,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &X1       SETA      &OLD
 &STACK    SETA      &X1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     255,'INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           ST        &X,&X1.(15)         X1 = (X)
           VSQR      &N,&X,&JX,&JX2,&J
           L         &X,&X1.(15)         &X = (X)
 &STACK    SETA      &OLD
           MEND

#endif
