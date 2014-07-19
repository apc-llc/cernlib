*
* $Id: register.inc,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: register.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_REGISTER_INC
#define CERNLIB_KERNNUM_REGISTER_INC
*
*
* register.inc
*
           MACRO
 &LABEL    CLEAR     &F
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RCLEAR
           AIF       ('&T' EQ 'D').DCLEAR
           AIF       ('&T' EQ 'C').CCLEAR
           MEXIT
 .CCLEAR   ANOP
           LD        &F+2,=D'0'          &F+2 = 0.
 .RCLEAR   ANOP
 .DCLEAR   ANOP
           LD        &F,=D'0'            &F = 0.
           MEND
           MACRO
 &LABEL    LOAD      &F,&X,&JX=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RLOAD
           AIF       ('&T' EQ 'D').DLOAD
           AIF       ('&T' EQ 'C').CLOAD
           MEXIT
 .CLOAD    LD        &F+2,=D'0'
           LE        &F+2,4(&X,&JX)
 .RLOAD    LD        &F,=D'0'
           LE        &F,0(&X,&JX)
           MEXIT
 .DLOAD    LD        &F,0(&X,&JX)
           MEND
           MACRO
 &LABEL    MINUS     &F
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RMINUS
           AIF       ('&T' EQ 'D').DMINUS
           AIF       ('&T' EQ 'C').CMINUS
           MEXIT
 .CMINUS   ANOP
           LCDR      &F+2,&F+2
 .RMINUS   ANOP
 .DMINUS   ANOP
           LCDR      &F,&F
           MEND
           MACRO
 &LABEL    MUL       &X
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RMUL
           AIF       ('&T' EQ 'D').DMUL
           AIF       ('&T' EQ 'C').CMUL
 .RMUL     ANOP
 .DMUL     ANOP
           M&W       0,0(&X)
           MEXIT
 .CMUL     ANOP
           L&W       4,&D.(&X)           F4  =  X"
           L&W       6,0(&X)             F6  =  X'
           M&W.R     4,2                 F4  =  R"*X"
           M&W.R     6,2                 F6  =  R"*X'
           LDR       2,0                 F2  =  R'
           M&W       0,0(&X)             F0  =  R'*X'
           M&W       2,&D.(&X)           F2  =  R'*X"
           S&W.R     0,4                 F0  =  R'*X' - R"*X"
           A&W.R     2,6                 F2  =  R'*X" + R"*X'
           MEND
           MACRO
 &LABEL    RCPRCL    &X
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RRCPRCL
           AIF       ('&T' EQ 'D').DRCPRCL
           AIF       ('&T' EQ 'C').CRCPRCL
           MEXIT
 .RRCPRCL  ANOP
 .DRCPRCL  ANOP
           LD        0,=D'1.0'           F0 = 1.
           D&W       0,0(&X)             F0 = 1./X
           ST&W      0,0(&X)             X  = 1./X
           MEXIT
 .CRCPRCL  ANOP
           SDR       0,0                 F0 = 0
           SDR       2,2                 F2 = 0
           L&W       0,0(&X)             F0 = X'
           L&W       2,&D.(&X)           F2 = X''
           LDR       4,0                 F4 = X'
           LDR       6,2                 F6 = X''
           M&W.R     4,4
           M&W.R     6,6
           ADR       4,6                 F4 = X'**2 + X''**2 = D
           D&W.R     0,4                 F0 = X' / D
           D&W.R     2,4                 F2 = X''/ D
           LC&W.R    2,2                 F2 = -X''/ D
           ST&W      0,0(&X)
           ST&W      2,&D.(&X)
           MEND
           MACRO
 &LABEL    STORE     &F,&X,&JX=0
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RSTORE
           AIF       ('&T' EQ 'D').DSTORE
           AIF       ('&T' EQ 'C').CSTORE
           MEXIT
 .CSTORE   ANOP
           ST&W      &F+2,&D.(&X,&JX)
 .RSTORE   ANOP
 .DSTORE   ANOP
           ST&W      &F,0(&X,&JX)
           MEND
           MACRO
 &LABEL    SWAP      &R1,&R2
 &LABEL    LR        0,&R1
           LR        &R1,&R2             &R1 = TWO
           LR        &R2,0               &R2 = ONE
           MEND

#endif
