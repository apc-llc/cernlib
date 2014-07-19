*
* $Id: mul.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: mul.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_MUL_INC
#define CERNLIB_KERNNUM_MUL_INC
*
*
* mul.inc
*
           MACRO
 &LABEL    MUL       &X,&Y,&Z,&JX=0,&JY=0,&JZ=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RMUL
           AIF       ('&T' EQ 'D').DMUL
           AIF       ('&T' EQ 'C').CMUL
           MNOTE     13,'NO TYPE DEFINED FOR MACRO MUL'
           MEXIT
 .RMUL     ANOP
 .DMUL     ANOP
           L&W       0,0(&X,&JX)         F0 = X
           M&W       0,0(&Y,&JY)         F0 = XY
           ST&W      0,0(&Z,&JZ)         Z  = XY
           MEXIT
 .CMUL     ANOP
           LE        0,0(&X,&JX)
           LE        2,&D.(&X,&JX)
           ME        0,&D.(&Y,&JY)       F0 = X'Y''
           ME        2,0(&Y,&JY)         F2 = X''Y'
           AER       0,2                 F0 = X'Y''+X''Y'
           STE       0,&D.(&Z,&JZ)
           LE        0,0(&X,&JX)
           LE        2,&D.(&X,&JX)
           ME        0,0(&Y,&JY)         F0 = X'Y'
           ME        2,&D.(&Y,&JY)       F2 = X''Y''
           SER       0,2                 F0 = X'Y'-X''Y''
           STE       0,0(&Z,&JZ)
           MEND
           MACRO
 &LABEL    VMUL      &N,&X,&JX,&Y,&JY,&Z,&JZ,&JX2,&JY2,&JZ2,&J
 &LABEL    FIRST     &J,&N,LOOP=L&SYSNDX
           MUL       &X,&Y,&Z
           AR        &X,&JX              (X) = (X) + JX
           AR        &Y,&JY              (Y) = (Y) + JY
           AR        &Z,&JZ              (Z) = (Z) + JZ
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           MUL       &X,&Y,&Z
           MUL       &X,&Y,&Z,JX=&JX,JY=&JY,JZ=&JZ
           AR        &X,&JX2             (X) = (X) + 2*JX
           AR        &Y,&JY2             (Y) = (Y) + 2*JY
           AR        &Z,&JZ2             (Z) = (Z) + 2*JZ
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND

#endif
