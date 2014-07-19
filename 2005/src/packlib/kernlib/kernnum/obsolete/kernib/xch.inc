*
* $Id: xch.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: xch.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_XCH_INC
#define CERNLIB_KERNNUM_XCH_INC
*
*
* xch.inc
*
           MACRO
 &LABEL    XCH       &X,&Y,&JX=0,&JY=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RXCH
           AIF       ('&T' EQ 'D').DXCH
           AIF       ('&T' EQ 'C').CXCH
           MNOTE     13,'NO TYPE DEFINED FOR XCH MACRO'
           MEXIT
 .CXCH     ANOP
           LD        0,0(&X,&JX)
           LD        2,0(&Y,&JY)
           STD       0,0(&Y,&JY)
           STD       2,0(&X,&JX)
           MEXIT
 .RXCH     ANOP
 .DXCH     ANOP
           L&W       0,0(&X,&JX)
           L&W       2,0(&Y,&JY)
           ST&W      0,0(&Y,&JY)
           ST&W      2,0(&X,&JX)
           MEND
           MACRO
 &LABEL    VXCH2     &N,&X,&JX,&Y,&JY,&JX2,&JY2
#include "kernnum/global.inc"
 &LABEL    ONCE      &N,L&SYSNDX
           XCH       &X,&Y
           AR        &X,&JX
           AR        &Y,&JY
 L&SYSNDX  LOOPTWO   &N,X&SYSNDX
           XCH       &X,&Y
           XCH       &X,&Y,JX=&JX,JY=&JY
           AR        &X,&JX2
           AR        &Y,&JY2
 X&SYSNDX  LEND      &N,L&SYSNDX
           MEND
           MACRO
 &LABEL    VXCH      &N,&X,&JX,&Y,&JY,&JX2,&JY2,&J
 &LABEL    FIRST     &J,&N,LOOP=L&SYSNDX
           XCH       &X,&Y
           AR        &X,&JX                        (X) = (X) + JX
           AR        &Y,&JY                        (Y) = (Y) + JY
 L&SYSNDX  FORTWO    &J,&N,EXIT=X&SYSNDX
           XCH       &X,&Y
           XCH       &X,&Y,JX=&JX,JY=&JY
           AR        &X,&JX2                       (X) = (X) + 2*JX
           AR        &Y,&JY2                       (Y) = (Y) + 2*JY
 X&SYSNDX  ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVXCH2    &N,&X,&JX,&Y,&JY,&JX2,&JY2,&J
#include "kernnum/global.inc"
           LCLA      &OLD,&X1,&Y1
 &LABEL    DS        0H
 &OLD      SETA      &STACK
 &X1       SETA      &OLD
 &Y1       SETA      &X1+1*4
 &STACK    SETA      &Y1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     255,'INSUFFICIENT WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           ST        &X,&X1.(15)         X1 = (X)
           ST        &Y,&Y1.(15)         Y1 = (Y)
           VXCH      &N,&X,&JX,&Y,&JY,&JX2,&JY2,&J
           L         &X,&X1.(15)         &X = (X)
           L         &Y,&Y1.(15)         &Y = (Y)
 &STACK    SETA      &OLD
           MEND

#endif
