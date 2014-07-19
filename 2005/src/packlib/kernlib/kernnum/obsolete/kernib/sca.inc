*
* $Id: sca.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: sca.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_SCA_INC
#define CERNLIB_KERNNUM_SCA_INC
*
*
* sca.inc
*
           MACRO
 &LABEL    SCA       &F,&X,&Y,&Z,&JX=0,&JY=0,&JZ=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RSCA
           AIF       ('&T' EQ 'D').DSCA
           AIF       ('&T' EQ 'C').CSCA
           MEXIT
 .RSCA     ANOP
 .DSCA     ANOP
           LDR       0,4                 F0=S
           M&W       0,0(&X,&JX)
           &F&W      0,0(&Y,&JY)         F0=SX+Y
           ST&W      0,0(&Z,&JZ)
           MEXIT
 .CSCA     ANOP
           SDR       0,0
           SDR       2,2
           L&W       0,0(&X,&JX)         F0 = X'
           L&W       2,&D.(&X,&JX)       F2 = X''
           M&W.R     0,4                 F0 = S'*X'
           M&W.R     2,6                 F2 = S''*X''
           S&W.R     0,2                 F0 = S'*X' - S''*X''
           &F&W      0,0(&Y,&JY)         F0=S'X'-S''X''+Y'
           SDR       2,2
           L&W       2,0(&X,&JX)         F2=X'
           ST&W      0,0(&Z,&JZ)         Z' = S'*X' - S''*X'' + Y'
           SDR       0,0
           L&W       0,&D.(&X,&JX)       F0=X''
           M&W.R     0,4                 F0 = S'*X''
           M&W.R     2,6                 F2 = S''*X'
           A&W.R     0,2                 F0 = S'*X'' + S''*X'
           &F&W      0,&D.(&Y,&JY)       F0 = S'*X'' + S''*X' + Y''
           ST&W      0,&D.(&Z,&JZ)       Z''= S'*X'' + S''*X' + Y''
           MEND
           MACRO
 &LABEL    VSCA      &F,&N,&X,&JX,&Y,&JY,&Z,&JZ,&JX2,&JY2,&JZ2,&J
 &LABEL    FIRST     &J,&N,LOOP=L&SYSNDX
           SCA       &F,&X,&Y,&Z
           AR        &X,&JX                        (X) = (X) + JX
           AR        &Y,&JY                        (Y) = (Y) + JY
           AR        &Z,&JZ                        (Z) = (Z) + JZ
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           SCA       &F,&X,&Y,&Z
           SCA       &F,&X,&Y,&Z,JX=&JX,JY=&JY,JZ=&JZ
           AR        &X,&JX2                       (X) = (X) + 2*JX
           AR        &Y,&JY2                       (Y) = (Y) + 2*JY
           AR        &Z,&JZ2                       (Z) = (Z) + 2*JZ
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVSCA2    &N,&X,&JX,&Y,&JY,&Z,&JZ,&JX2,&JY2,&JZ2,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X1,&Y1,&Z1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &X1       SETA      &OLD
 &Y1       SETA      &X1+1*4
 &Z1       SETA      &Y1+1*4
 &STACK    SETA      &Z1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     255,'INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           ST        &X,&X1.(15)         X1 = (X)
           ST        &Y,&Y1.(15)         Y1 = (Y)
           ST        &Z,&Z1.(15)         Z1 = (Z)
           VSCA      &F,&N,&X,&JX,&Y,&JY,&Z,&JZ,&JX2,&JY2,&JZ2,&J
           L         &X,&X1.(15)         &X = (X)
           L         &Y,&Y1.(15)         &Y = (Y)
           L         &Z,&Z1.(15)         &Z = (Z)
 &STACK    SETA      &OLD
           MEND

#endif
