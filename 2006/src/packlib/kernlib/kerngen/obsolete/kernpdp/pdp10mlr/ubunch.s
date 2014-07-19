*
* $Id: ubunch.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: ubunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE  UBUNCH  (IN,OUT,NCH)
;#
; CERN PROGLIB# M409    UBUNCH          .VERSION KERNPDP  1.01  760321
;#
;      NEW VERSION: CLEAR BIT 35, SPEED.   D.BERTRAND, JZ
       A1=1
       A2=2
       JIN=3
       JOUT=4
       NCH=5
       NEND=6
       NSTOP=7
;--
       SEARCH HELP
#include "reentr.inc"
       HELLO (UBUNCH)
;--
       HRRZI   JIN,@(16)       ;JIN = ADR(IN(1))
       HRRZI   JOUT,@1(16)     ;JOUT = ADR(OUT(1))
       MOVE    NCH,@2(16)      ;NCH
       CAIL    NCH,5
       JRST    CH5M            ; JUMP FOR NCH .GE. 5
       JUMPL   NCH,BRANCH      ; JUMP FOR NCH .LT. 0
       MOVE    A1,BLANK
       JRST    @BRANCH(NCH)
;--
;----          NCH = 1, 2, 3, 4  CHARACTERS
CH1:   MOVE    A2,(JIN)
       LSHC    A1,7
       ROT     A1,-7
       JRST    RETURN
;--
CH2:   MOVE    A2,(JIN)
       LSHC    A1,7
       MOVE    A2,1(JIN)
       LSHC    A1,7
       ROT     A1,-16
       JRST    RETURN
;--
CH3:   MOVE    A2,(JIN)
       LSHC    A1,7
       MOVE    A2,1(JIN)
       LSHC    A1,7
       MOVE    A2,2(JIN)
       LSHC    A1,7
       ROT     A1,17
       JRST    RETURN
;--
CH4:   MOVE    A2,(JIN)
       LSHC    A1,7
       MOVE    A2,1(JIN)
       LSHC    A1,7
       MOVE    A2,2(JIN)
       LSHC    A1,7
       MOVE    A2,3(JIN)
       LSHC    A1,7
       ROT     A1,10
       JRST    RETURN
;--
;----          NCH = 5 OR MORE CHARACTERS
CH5M:  MOVE    NEND,JIN
       ADD     NEND,NCH        ;NEND = JIN+NCH = ADR(IN(NCH+1))
       MOVEI   NSTOP,-4(NEND)  ;NSTOP= JIN+NCH-4 = LOOP STOP ADR
LOOP:  MOVE    A2,(JIN)
       LSHC    A1,7
       MOVE    A2,1(JIN)
       LSHC    A1,7
       MOVE    A2,2(JIN)
       LSHC    A1,7
       MOVE    A2,3(JIN)
       LSHC    A1,7
       MOVE    A2,4(JIN)
       LSHC    A1,7
       LSH     A1,1
       MOVEM   A1,(JOUT)
;--
       ADDI    JOUT,1          ; BUMP JOUT: ADR (OUT(NEXT)
       ADDI    JIN,5           ; BUMP JIN : ADR (IN(NEXT))
       CAMGE   JIN,NSTOP
       JRST    LOOP            ; LOOP UNTIL JIN .GE. NSTOP
       MOVE    NCH,NEND
       SUB     NCH,JIN         ;NCH = NEND-JIN = NO OF CHAR. REMAINING
       MOVE    A1,BLANK
       JRST    @BRANCH(NCH)
;--
RETURN:MOVEM    A1,(JOUT)
CH0:   GOODBY
;--
BLANK: ASCII/     /
BRANCH:XWD     0,CH0
       XWD     0,CH1
       XWD     0,CH2
       XWD     0,CH3
       XWD     0,CH4
       PRGEND
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
