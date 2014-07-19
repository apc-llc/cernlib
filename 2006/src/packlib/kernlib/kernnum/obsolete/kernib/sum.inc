*
* $Id: sum.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: sum.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_SUM_INC
#define CERNLIB_KERNNUM_SUM_INC
*
*
* sum.inc
*
           MACRO
 &LABEL    SUM       &X,&JX=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RSUM
           AIF       ('&T' EQ 'D').DSUM
           AIF       ('&T' EQ 'C').CSUM
           MNOTE     13,'NO TYPE DECLARED FOR SUM MACRO'
           MEXIT
 .CSUM     A&W       2,&D.(&X,&JX)       F2 = F2+X''
 .RSUM     ANOP
 .DSUM     ANOP
           A&W       0,0(&X,&JX)         F0 = F0+X'
           MEND
           MACRO
 &LABEL    VSUM      &N,&X,&JX,&JX2,&J
 &LABEL    CLEAR     0
           FIRST     &J,&N,LOOP=L&SYSNDX
           SUM       &X
           AR        &X,&JX                        (X) = (X) + JX
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           SUM       &X
           SUM       &X,JX=&JX
           AR        &X,&JX2                       (X) = (X) + 2*JX
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND

#endif
