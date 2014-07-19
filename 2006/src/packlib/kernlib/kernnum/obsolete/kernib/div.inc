*
* $Id: div.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: div.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_DIV_INC
#define CERNLIB_KERNNUM_DIV_INC
*
*
* div.inc
*
           MACRO
 &LABEL    DIV       &X,&Y,&Z,&JX=0,&JY=0,&JZ=0,&EXIT=
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RDIV
           AIF       ('&T' EQ 'D').DDIV
           AIF       ('&T' EQ 'C').CDIV
           MNOTE     13,'NO TYPE DEFINED FOR MACRO DIV'
           MEXIT
 .RDIV     LE        0,0(&X,&JX)         F0 = X
           LE        2,0(&Y,&JY)         F2 = Y
           CE        2,=F'0'
           BE        &EXIT               IF(Y .EQ. 0.)
           DER       0,2                 F0 = X/Y
           STE       0,0(&Z,&JZ)         Z  = X/Y
           MEXIT
 .DDIV     LD        0,0(&X,&JX)         F0 = X
           LD        2,0(&Y,&JY)         F2 = Y
           CD        2,=D'0'
           BE        &EXIT               IF(Y .EQ. 0.)
           DDR       0,2                 F0 = X/Y
           STD       0,0(&Z,&JZ)         Z  = X/Y
           MEXIT
 .CDIV     DS        0H
           LE        0,0(&Y,&JY)         F0 = Y'
           LE        2,&D.(&Y,&JY)       F2 = Y''
           LPER      4,0                 F4 = ABS(Y')
           LPER      6,2                 F6 = ABS(Y'')
           CER       4,6                 IF(ABS(Y') .GT. ABS(Y''))
           BH        A&SYSNDX
           CE        2,=F'0'             IF(Y'' .EQ. 0)
           BE        &EXIT
           LER       6,0                 F6 = Y'
           DER       6,2                 F6 = Y'/Y'' = R
           MER       0,6                 F0 = Y'*R
           AER       2,0                 F2 = Y'*R+Y''
           LE        0,0(&X,&JX)         F0 = X'
           LE        4,&D.(&X,&JX)       F4 = X''
           MER       0,6                 F0 = X'*R
           MER       6,4                 F6 = X''*R
           AER       0,4                 F0 = X'*R+X''
           SE        6,0(&X,&JX)         F6 = X''*R-X'
           DER       0,2                 F0 = (X'*R+X'') / (Y'*R+Y'')
           DER       6,2                 F6 = (X''*R-X') / (Y'*R+Y'')
           STE       0,0(&Z,&JZ)         Z' = (X'*R+X'') / (Y'*R+Y'')
           STE       6,&D.(&Z,&JZ)       Z''= (X''*R-X') / (Y'*R+Y'')
           B         B&SYSNDX
 A&SYSNDX  LER       6,2                 F6 = Y''
           DER       6,0                 F6 = Y''/Y' = S
           MER       2,6                 F2 = Y''*S
           AER       0,2                 F0 = Y''*S+Y'
           LE        2,0(&X,&JX)         F2 = X'
           LE        4,&D.(&X,&JX)       F4 = X''
           MER       2,6                 F2 = X'*S
           MER       6,4                 F6 = X''*S
           SER       4,2                 F4 = X''-X'*S
           AE        6,0(&X,&JX)         F6 = X''*S+X'
           DER       4,0                 F4 = (X''-X'*S) / (Y''*S+Y')
           DER       6,0                 F6 = (X''*S+X') / (Y''*S+Y')
           STE       4,&D.(&Z,&JZ)       Z''= (X''-X'*S) / (Y''*S+Y')
           STE       6,0(&Z,&JZ)         Z' = (X''*S+X') / (Y''*S+Y')
 B&SYSNDX  DS        0H
           MEND
           MACRO
 &LABEL    VDIV      &N,&X,&JX,&Y,&JY,&Z,&JZ,&J
 &LABEL    DS        0H
 L&SYSNDX  FORONE    &J                  J = 1
           DIV       &X,&Y,&Z,EXIT=X&SYSNDX
           AR        &X,&JX              (X) = (X) + JX
           AR        &Y,&JY              (Y) = (Y) + JY
           AR        &Z,&JZ              (Z) = (Z) + JZ
           ENDFOR    &J,&N,LOOP=L&SYSNDX J = J+1
           LA        &J,0                J = 0
 X&SYSNDX  DS        0H
           MEND

#endif
