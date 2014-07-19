*
* $Id: mpy.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: mpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_MPY_INC
#define CERNLIB_KERNNUM_MPY_INC
*
*
* mpy.inc
*
           MACRO
 &LABEL    MPY       &F,&X,&Y,&JX=0,&JY=0
#include "kernnum/global.inc"
 *                                       F6 = 0. FOR TYPES R AND C
 &LABEL    DS        0H
           AIF       ('&TYPE' EQ '&RTYPE').RMPY
           AIF       ('&TYPE' EQ '&DTYPE').DMPY
           AIF       ('&TYPE' EQ '&CTYPE').CMPY
           AIF       ('&TYPE' EQ '&CTYPEC').CC
           AIF       ('&TYPE' EQ '&CCTYPE').CC
           MNOTE     12,'ILLEGAL TYPE FOR MPY MACRO'
           MEXIT
 .RMPY     ANOP
           LDR       4,2                 F4=0.
 .DMPY     L&W       4,0(&X,&JX)         F4=X
           M&W       4,0(&Y,&JY)         F4=XY
           &F.DR     0,4                 F0=S+-XY
           MEXIT
 .CMPY     LDR       4,6                 F4=0.
           LE        6,0(&X,&JX)         F6=X'
           LE        4,0(&X,&JX)         F4=X'
           ME        6,&D.(&Y,&JY)       F6=X'Y''
           ME        4,0(&Y,&JY)         F4=X'Y'
           &F.DR     2,6                 F2=F2+-X'Y''
           LD        6,=D'0'             F6=0.
           &F.DR     0,4                 F0=F0+-X'Y'
           LDR       4,6                 F4=0.
           LE        6,&D.(&X,&JX)       F6=X''
           LE        4,&D.(&X,&JX)       F4=X''
           ME        6,0(&Y,&JY)         F6=X''Y'
           ME        4,&D.(&Y,&JY)       F4=X''Y''
           AIF       ('&F' NE 'A').ALPHA
           ADR       2,6                 F2=F2+X'Y''+X''Y'
           LD        6,=D'0'             F6=0.
           SDR       0,4                 F0=F0+X'Y'-X''Y''
           AGO       .BETA
 .ALPHA    SDR       2,6                 F2=F2-X'Y''-X''Y'
           LD        6,=D'0'             F6=0.
           ADR       0,4                 F0=F0-X'Y'+X''Y''
 .BETA     ANOP
           MEXIT
 .CC       AIF       (&CC EQ 0).CMPY
           AIF       (&CC EQ 1).CMPY1
           AIF       (&CC EQ 2).CMPY2
           MNOTE     12,'ILLEGAL CC FOR MPY MACRO'
           MEXIT
 .CMPY1    LDR       4,6                 F4=0.
           LE        6,0(&X,&JX)         F6=X'
           LE        4,0(&X,&JX)         F4=X'
           ME        6,&D.(&Y,&JY)       F6=X'Y''
           ME        4,0(&Y,&JY)         F4=X'Y'
           &F.DR     2,6                 F2=F2+-X'Y''
           LD        6,=D'0'             F6=0.
           &F.DR     0,4                 F0=F0+-X'Y'
           LDR       4,6                 F4=0.
           LE        6,&D.(&X,&JX)       F6=X''
           LE        4,&D.(&X,&JX)       F4=X''
           ME        6,0(&Y,&JY)         F6=X''Y'
           ME        4,&D.(&Y,&JY)       F4=X''Y''
           AIF       ('&F' NE 'A').GAMMA
           SDR       2,6                 F2=F2+X'Y''-X''Y'
           LD        6,=D'0'             F6=0.
           ADR       0,4                 F0=F0+X'Y'+X''Y''
           AGO       .DELTA
 .GAMMA    ADR       2,6                 F2=F2-X'Y''+X''Y'
           LD        6,=D'0'             F6=0.
           SDR       0,4                 F0=F0-X'Y'-X''Y''
 .DELTA    ANOP
           MEXIT
 .CMPY2    LDR       4,6                 F4=0.
           LE        6,0(&X,&JX)         F6=X'
           LE        4,0(&X,&JX)         F4=X'
           ME        6,&D.(&Y,&JY)       F6=X'Y''
           ME        4,0(&Y,&JY)         F4=X'Y'
           AIF       ('&F' NE 'A').ZETA
           SDR       2,6                 F2=F2-X'Y''
           LD        6,=D'0'             F6=0.
           ADR       0,4                 F0=F0+X'Y'
           AGO       .ETA
 .ZETA     ADR       2,6                 F2=F2+X'Y''
           LD        6,=D'0'             F6=0.
           SDR       0,4                 F0=F0-X'Y'
 .ETA      ANOP
           LDR       4,6                 F4=0.
           LE        6,&D.(&X,&JX)       F6=X''
           LE        4,&D.(&X,&JX)       F4=X''
           ME        6,0(&Y,&JY)         F6=X''Y'
           ME        4,&D.(&Y,&JY)       F4=X''Y''
           &F.DR     2,6                 F2=F2-+X'Y''+-X''Y'
           LD        6,=D'0'             F6=0.
           &F.DR     0,4                 F0=F0+-X'Y'+-X''Y''
           MEND
           MACRO
 &LABEL    VMPY      &N,&X,&JX,&Y,&JY,&JX2,&JY2
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'D').ALPHA
           LD        6,=D'0'                 F6 = 0. (SEE MPY MACRO)
 .ALPHA    ANOP
           ONCE      &N,L&SYSNDX
           MPY       A,&X,&Y
           AR        &X,&JX                        (X) = (X) + JX
           AR        &Y,&JY                        (Y) = (Y) + JY
 L&SYSNDX  LOOPTWO   &N,X&SYSNDX
           MPY       A,&X,&Y
           MPY       A,&X,&Y,JX=&JX,JY=&JY
           AR        &X,&JX2                       (X) = (X) + 2*JX
           AR        &Y,&JY2                       (Y) = (Y) + 2*JY
 X&SYSNDX  LEND      &N,L&SYSNDX
           AIF       ('&T' EQ 'R').RROUND
           AIF       ('&T' EQ 'C').CROUND
           MEXIT
 .CROUND   LRER      2,2
 .RROUND   LRER      0,0
           MEND
           MACRO
 &LABEL    TVMPY2    &N,&X,&JX,&Y,&JY,&JX2,&JY2
#include "kernnum/global.inc"
 &LABEL    PUSH      &X                  SAVE (X(1))
           PUSH      &Y                       (Y(1))
           VMPY      &N,&X,&JX,&Y,&JY,&JX2,&JY2
           POP       &Y                  &Y = (Y(1))
           POP       &X                  &X = (X(1))
           MEND
           MACRO
 &LABEL    TXMPY     &OPC,&M,&N,&X,&IX,&JX,&Y,&JY,&Z,&IZ
#include "kernnum/global.inc"
           LCLA      &OLD,&Y1
 &LABEL    DS        0H
           LCLC      &F
 &F        SETC      'A'                 FOR &OPC = XMPY, XMPA, XMPS
           AIF       ('&OPC'(3,1) EQ 'P').ALPHA
 &F        SETC      'S'                 FOR &OPC = XMNY, XMNA, XMNS
 .ALPHA    ANOP                              WHERE X = M OR U
           AIF       ('&OPC'(1,1) NE 'U').BETA
           PUSH      &Y
           AGO       .DELTA
 .BETA     ANOP
 &OLD      SETA      &STACK
 &Y1       SETA      &OLD
 &STACK    SETA      &Y1+1*4
           AIF       (&STACK LE &STKLIM).GAMMA
           MNOTE     12,'NO WORK SPACE FOR TXMPY'
           MEXIT
 .GAMMA    ST        &Y,&Y1.(&BASEREG)   Y1 = (Y(1))
 .DELTA    ANOP
           AIF       ('&T' EQ 'D').EPSILON
           LD        6,=D'0'             F6 = 0.
 .EPSILON  ANOP
           PUSH      &M
           PUSH      &X
           PUSH      &Z
           LR        1,&N                R1 = N
           LA        0,1                 R0 = 1
           AIF       ('&T' NE 'C').ZETA
           CXMPY     &OPC,&F,&M,&N,&X,&IX,&JX,&Y1,&Y,&JY,&Z,&IZ
           AGO       .ETA
 .ZETA     RXMPY     &OPC,&F,&M,&N,&X,&IX,&JX,&Y1,&Y,&JY,&Z,&IZ
 .ETA      ANOP
           POP       &Z                  &Z = (Z(1))
           POP       &X                  &X = (X(1,1))
           POP       &M                  &M = M
           AIF       ('&OPC'(1,1) NE 'U').THETA
           POP       &Y                  &Y = (Y(1))
           AGO       .IOTA
 .THETA    LR        &N,1                &N = N
           L         &Y,&Y1.(&BASEREG)   &Y = (Y(1))
 &STACK    SETA      &OLD
 .IOTA     ANOP
           MEND
           MACRO
 &LABEL    CXMPY     &OPC,&F,&M,&N,&X,&IX,&JX,&Y1,&Y,&JY,&Z,&IZ
 *                                                 R0=1, R1=N
 *                                                 F6=0.
#include "kernnum/global.inc"
 &LABEL    DS        0H
 A&SYSNDX  PUSH      &X                  STACK (X(I,...))
           AIF       ('&OPC'(1,1) NE 'U').ALPHA
           PUSH      &Y                  STACK (Y(I))
 .ALPHA    ANOP
           LDR       0,6                 F0 = 0.
           LDR       2,6                 F2 = 0.
           AIF       ('&OPC'(4,1) EQ 'Y').BETA
           LE        0,0(&Z)             F0 = Z(I)'
           LE        2,4(&Z)             F2 = Z(I)''
           AIF       ('&OPC'(4,1) EQ 'A').BETA
           LCDR      0,0                 F0 = -Z(I)'
           LCDR      2,2                 F2 = -Z(I)''
 .BETA     ANOP
           LA        &N,1                &N = J = 1
 B&SYSNDX  MPY       &F,&X,&Y
           AR        &X,&JX              &X = (X(I,J+1))
           AR        &Y,&JY              &Y = (Y(J+1))
           BXLE      &N,0,B&SYSNDX
           LRER      0,0
           LRER      2,2
           STE       0,0(&Z)
           STE       2,4(&Z)
           AIF       ('&OPC'(1,1) NE 'U').GAMMA
           POP       &Y                  &Y = (Y(I))
           AGO       .DELTA
 .GAMMA    L         &Y,&Y1.(&BASEREG)   &Y = (Y(1))
 .DELTA    POP       &X                  &X = (X(I,...))
           AR        &Z,&IZ
           AR        &X,&IX
           AIF       ('&OPC'(1,1) NE 'U').EPSILON
           AR        &Y,&JY              &Y = (Y(I+1))
           AR        &X,&JX              &X = (X(I+1,I+1))
           SR        1,0                 N  = N-1
           AGO       .ZETA
 .EPSILON  SR        &M,0                M  = M-1
 .ZETA     BNZ       A&SYSNDX
           MEND
           MACRO
 &LABEL    RXMPY     &OPC,&F,&M,&N,&X,&IX,&JX,&Y1,&Y,&JY,&Z,&IZ
 *                                       R0=1, R1=N
 *                                       F6=0. FOR TYPE R
#include "kernnum/global.inc"
 &LABEL    DS        0H
 A&SYSNDX  DS        0H                  BEGIN MAIN LOOP
           AIF       ('&OPC'(1,1) NE 'U').ALPHA
           SR        1,0                 N  = N-1
           AGO       .BETA
 .ALPHA    SR        &M,0                M  = M-1
 .BETA     BZ        C&SYSNDX            IF ONE ROW IS LEFT OVER
           AIF       ('&T' EQ 'R').DELTA
           AIF       ('&OPC'(4,1) NE 'Y').EPSILON
           LD        0,=D'0'             F0 = 0.
           LDR       2,0                 F2 = 0.
           AGO       .ZETA
 .DELTA    LDR       0,6                 F0 = 0.
           LDR       2,6                 F2 = 0.
           AIF       ('&OPC'(4,1) EQ 'Y').ZETA
 .EPSILON  L&W       0,0(&Z)             F0 = Z(1)
           L&W       2,0(&Z,&IZ)         F2 = Z(2)
           AIF       ('&OPC'(4,1) EQ 'A').ZETA
           LCDR      0,0                 F0 = -Z(1)
           LCDR      2,2                 F2 = -Z(2)
 .ZETA     ANOP
           AIF       ('&OPC'(1,1) NE 'U').ETA
           MPY       &F,&X,&Y            F0 = F0+X(1,1)*Y(1)
           AR        &X,&JX              &X = (X(1,2))
           AR        &Y,&JY              &Y = (Y(2))
           PUSH      &Y                  STACK (Y(2))
 .ETA      PUSH      &X                  STACK (X)
           LA        &N,1                &N = J = 1
 B&SYSNDX  DS        0H
           AIF       ('&T' NE 'R').THETA
           LDR       4,6                 F4 = 0.
 .THETA    L&W       6,0(&Y)             F6 = Y(J)
           L&W       4,0(&X)             F4 = X(1,J)
           M&W.R     4,6                 F4 = X(1,J)*Y(J)
           M&W       6,0(&X,&IX)         F6 = X(2,J)*Y(J)
           &F.DR     0,4                 F0 = F0+-X(1,J)*Y(J)
           &F.DR     2,6                 F2 = F2+-X(2,J)*Y(J)
           AIF       ('&T' NE 'R').IOTA
           LD        6,=D'0'             F6 = 0.
 .IOTA     AR        &X,&JX              &X = (X(1,J+1))
           AR        &Y,&JY              &Y = (Y(J+1))
           BXLE      &N,0,B&SYSNDX
           AIF       ('&T' NE 'R').KAPPA
           LRER      0,0
           LRER      2,2
 .KAPPA    ST&W      0,0(&Z)             Z(1) = F0
           ST&W      2,0(&Z,&IZ)         Z(2) = F2
           POP       &X                  &X = (X(I,...))
           AIF       ('&OPC'(1,1) NE 'U').LAMBDA
           POP       &Y                  &Y = (Y(2))
           AGO       .MU
 .LAMBDA   L         &Y,&Y1.(&BASEREG)   &Y = (Y(1))
 .MU       ANOP
           AR        &Z,&IZ
           AR        &X,&IX
           AR        &Z,&IZ              &Z = (Z(I+2))
           AR        &X,&IX              &X = (X(I+2,...))
           AIF       ('&OPC'(1,1) NE 'U').NU
           AR        &Y,&JY              &Y = (Y(I+2))
           AR        &X,&JX              &X = (X(I+2,I+2))
           SR        1,0                 N  = N-2
           AGO       .XI
 .NU       SR        &M,0                M  = M-2
 .XI       BNZ       A&SYSNDX
           B         E&SYSNDX            IF ALL DONE
 C&SYSNDX  DS        0H                  LAST ROW
           AIF       ('&T' EQ 'R').OMICRON
           AIF       ('&OPC'(4,1) NE 'Y').PI
           LD        0,=D'0'             F0 = 0.
           AGO       .RHO
 .OMICRON  LDR       0,6                 F0 = 0.
           AIF       ('&OPC'(4,1) EQ 'Y').RHO
 .PI       L&W       0,0(&Z)
           AIF       ('&OPC'(4,1) EQ 'A').RHO
           LCDR      0,0                 F0 = -Z(1)
 .RHO      ANOP
           AIF       ('&OPC'(1,1) NE 'U').SIGMA
           MPY       &F,&X,&Y
           AGO       .TAU
 .SIGMA    LA        &N,1                &N = J = 1
 D&SYSNDX  MPY       &F,&X,&Y
           AR        &X,&JX              &X = (X(M,J+1))
           AR        &Y,&JY              &Y = (Y(J+1))
           BXLE      &N,0,D&SYSNDX
 .TAU      AIF       ('&T' NE 'R').YPSILON
           LRER      0,0
 .YPSILON  ST&W      0,0(&Z)             Z(1) = X(1,1)*Y(1)+...
 E&SYSNDX  DS        0H
           MEND
           MACRO
 &LABEL    TMBIL     &N,&X,&IX,&Y,&IY,&JY,&Z,&JZ,&JY2,&JZ2
#include "kernnum/global.inc"
           LCLA      &OLD,&I1,&FCN1,&FCN2,&X1,&Y11
 &OLD      SETA      &STACK
 &I1       SETA      &OLD
 &FCN1     SETA      &I1+1*4
 &FCN2     SETA      &FCN1+1*4
 &X1       SETA      &FCN2+1*4
 &Y11      SETA      &X1+1*4
 &STACK    SETA      &Y11+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     13,'NO SPACE FOR MACRO TMBIL'
           MEXIT
 .ALPHA    ANOP
 &LABEL    LR        &JY2,&JY
           LR        &JZ2,&JZ
           AR        &JY2,&JY            &JY2 = 2*JY
           AR        &JZ2,&JZ            &JZ2 = 2*JZ
           ST        &X,&X1.(15)         X1 = (X)
           ST        &Y,&Y11.(15)        Y1 = (Y)
           ST&W      0,&FCN1.(15)        FCN= 0.
           AIF       ('&T' NE 'C').BETA
           STE       2,&FCN2.(15)
 .BETA     ANOP
           LR        0,&N                R0 = N
 A&SYSNDX  ST        0,&I1.(15)          I1 = N-I
           CLEAR     0                   F0 [,F2] = 0.
           TVMPY2    &N,&Y,&JY,&Z,&JZ,&JY2,&JZ2
           MUL       &X                  F0 = X(I)*(Y(I,1)*Z(1)+...
           AIF       ('&T' EQ 'R').RBIL
           AIF       ('&T' EQ 'D').DBIL
           AIF       ('&T' EQ 'C').CBIL
           MNOTE     13,'INCORRECT TYPE FOR TMBIL'
           MEXIT
 .CBIL     AE        2,&FCN2.(15)        F2 = SUM + X(I)*(...)
           STE       2,&FCN2.(15)        SUM= SUM + X(I)*(...)
 .RBIL     ANOP
 .DBIL     A&W       0,&FCN1.(15)
           ST&W      0,&FCN1.(15)
           AR        &Y,&IY              &Y = (Y(I+1,1))
           AR        &X,&IX              &X = (X(I+1))
           L         0,&I1.(15)          R0 = N-I
           S         0,=F'1'
           BNZ       A&SYSNDX            WHILE I<N
           L         &X,&X1.(15)         &X = (X(1))
           L         &Y,&Y11.(15)        &Y = (Y(1,1))
 &STACK    SETA      &OLD
           MEND

#endif
