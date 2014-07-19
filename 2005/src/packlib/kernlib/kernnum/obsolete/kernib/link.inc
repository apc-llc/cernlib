*
* $Id: link.inc,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: link.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_LINK_INC
#define CERNLIB_KERNNUM_LINK_INC
*
*
* link.inc
*
           MACRO
           HEADER    &ENTRY
#include "kernnum/global.inc"
 &T&NAME   START
 &T&NAME   AMODE     ANY
 &T&NAME   RMODE     ANY
 BASE      EQU       *
           USING     BASE,&BASEREG
 EQ        EQU       8
 NE        EQU       7
 NZ        EQU       6
 LT        EQU       4
 LE        EQU       12
 GT        EQU       2
 GE        EQU       10
 REG14     EQU       3*4
 REG0      EQU       5*4
           B         &ENTRY
           DC        XL1'6',C'&T&NAME'
 &STKORG   SETA      12        FIRST FULL WORD AFTER TRACE WORD
 &STACK    SETA      12
 &STKLIM   SETA      12
 .ALPHA    AIF       (&TWORDS EQ 0).BETA
           DS        0F                  TO FORCE ALIGNMENT
           DS        1F                  WORK SPACE FOR ALL MACROS
 &STKLIM   SETA      &STKLIM+4           LIMIT ADDRESS OF WORK SPACE
 &TWORDS   SETA      &TWORDS-1
           AGO       .ALPHA
 .BETA     ANOP
           MEND
           MACRO
 &ENTRY    PROLOG
#include "kernnum/global.inc"
           AIF       (&REGLIM LT 13).ALPHA
           MNOTE     12,'USE OF REGISTER 13 IS ILLEGAL'
           MEXIT
 .ALPHA    ANOP
 &ENTRY    STM       14,&REGLIM,REG14(13)
           MEND
           MACRO
 &EXIT     EPILOG
#include "kernnum/global.inc"
 &EXIT     DS        0H                  TO FORCE ALIGNMENT
           AIF       (&REGLIM LT 13).BETA
           MNOTE     12,'USE OF REGISTER 13 IS ILLEGAL'
           MEXIT
 .BETA     ANOP
           MVI       REG14(13),EXITAG
           LM        0,&REGLIM,REG0(13)
           BR        14
 .GAMMA    ANOP
 EXITAG    EQU       255
           AIF       (&STACK EQ &STKORG).OMEGA
           MNOTE     12,'WORK SPACE ALLOCATION ERROR'
 .OMEGA    ANOP
           END
           MEND
           MACRO
 &LABEL    ENTER     &SUB,&LIST
#include "kernnum/global.inc"
           LCLA      &OLD,&SAVE,&R1,&R12
 &OLD      SETA      &STACK
 &SAVE     SETA      &OLD                18-WORD SAVE AREA
 &R1       SETA      &SAVE+18*4          SAVE OF R1
 &R12      SETA      &R1+1*4             SAVE OF R12-14
 &STACK    SETA      &R12+3*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     12,'ENTER MACRO LACKS WORK SPACE'
           MEXIT
 .ALPHA    ANOP
           ST        1,&R1.(&BASEREG)    SAVE R1
           STM       12,14,&R12.(&BASEREG) SAVE R12-14
           LR        12,&BASEREG         SAVE BASEREG IN R12
           LA        1,&LIST             R1=(ARGUMENT LIST)
           LA        13,&SAVE.(&BASEREG) R13=(18-WORD SAVE AREA)
           L         15,&SUB             R15=(ENTRY POINT)
           BALR      14,15               ENTER SUBPROGRAM
           LR        &BASEREG,12         RESTORE BASE REGISTER
           L         1,&R1.(&BASEREG)    RESTORE OLD R1
           LM        12,14,&R12.(&BASEREG) RESTORE OLD R12-14
 &STACK    SETA      &OLD                RELEASE SAVE AREA
           MEND

#endif
