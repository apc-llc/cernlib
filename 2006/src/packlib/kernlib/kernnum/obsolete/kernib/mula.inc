*
* $Id: mula.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: mula.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_MULA_INC
#define CERNLIB_KERNNUM_MULA_INC
*
*
* mula.inc
*
           MACRO
 &LABEL    MULA      &F,&X,&Y,&Z,&JX=0,&JY=0,&JZ=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RMULA
           AIF       ('&T' EQ 'D').DMULA
           AIF       ('&T' EQ 'C').CMULA
           MNOTE     13,'NO TYPE DEFINED FOR MACRO MULA'
           MEXIT
 .RMULA    ANOP
 .DMULA    ANOP
           L&W       0,0(&X,&JX)         F0 = X
           L&W       2,0(&Z,&JZ)         F2 = Z
           M&W       0,0(&Y,&JY)         F0 = XY
           &F&W.R    2,0                 F2 = Z+/-XY
           ST&W      2,0(&Z,&JZ)         Z = Z+/-XY
           MEXIT
 .CMULA    ANOP
           LE        0,0(&X,&JX)         F0=X'
           LE        2,&D.(&X,&JX)       F2=X''
           ME        0,&D.(&Y,&JY)       F0 = X'Y''
           ME        2,0(&Y,&JY)         F2=X''Y'
           AER       0,2                 F0=X'Y''+X''Y'
           LE        2,&D.(&Z,&JZ)       F2=Z''
           &F.ER     2,0                 F2 = Z''+\-(X'Y''+X''Y')
           STE       2,&D.(&Z,&JZ)
           LE        0,0(&X,&JX)         F0 = X'
           LE        2,&D.(&X,&JX)       F2 = X''
           ME        0,0(&Y,&JY)         F0 = X'Y'
           ME        2,&D.(&Y,&JY)       F2 - X''Y''
           SER       0,2                 F0 = X'Y'-X''Y''
           LE        2,0(&Z,&JZ)         F2 = Z'
           &F.ER     2,0                 F2= Z'+/-(X'Y'-X''Y'')
           STE       2,0(&Z,&JZ)
           MEND
           MACRO
 &LABEL    VMULA     &F,&N,&X,&JX,&Y,&JY,&Z,&JZ,&JX2,&JY2,&JZ2,&J
 &LABEL    FIRST     &J,&N,LOOP=L&SYSNDX
           MULA      &F,&X,&Y,&Z
           AR        &X,&JX              (X) = (X) + JX
           AR        &Y,&JY              (Y) = (Y) + JY
           AR        &Z,&JZ              (Z) = (Z) + JZ
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           MULA      &F,&X,&Y,&Z
           MULA      &F,&X,&Y,&Z,JX=&JX,JY=&JY,JZ=&JZ
           AR        &X,&JX2             (X) = (X) + 2*JX
           AR        &Y,&JY2             (Y) = (Y) + 2*JY
           AR        &Z,&JZ2             (Z) = (Z) + 2*JZ
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND

#endif
