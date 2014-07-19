*
* $Id: uctoh.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: uctoh.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   UCTOH
;
; CERN PROGLIB# M409    UCTOH           .VERSION KERNPDP  2.03  841128
; ORIG.   J. MOREELS, I.I.H.E. BRUSSELS, 6-NOV-84
;
ENTRY UCTOH
 
;       SUBROUTINE UCTOH(VC,VJ,NH,NCH)
 
        SIXBIT /UCTOH/
UCTOH:  MOVEI   5,@1(16)        ;GET THE ARGUMENTS
        MOVEM   5,VJ
        MOVE    6,@2(16)
        CAILE   6,5             ;MAXIMUM 5 CHAR/WORD
        MOVEI   6,5
        MOVEM   6,J
        MOVE    7,@3(16)
        MOVEM   7,NCH
        JUMPLE  7,END           ;IF NCH<=0 RETURN
        MOVE    1,(16)          ;GET THE INPUT STRING
        LDB     2,[270400,,1]   ;GET ARGUMENT TYPE
        CAIN    2,15            ;SKIP IF HOLLERITH CODE
        JRST    CHAR            ;HANDLE CHARACTER TYPE
        MOVEI   1,@1            ;SET-UP CALLING SEQUENCE FOR UTRANS
        HRLI    1,440700        ;SET-UP POINTER FOR HOLLERITH
        JRST    HOLL
CHAR:   DMOVE   1,@1            ;GET ILDB POINTER AND STRING
                                ;LENGHT
        CAMGE   2,7             ;IF THE CHARACTER STRING IS
        MOVE    7,2             ;SHORTER COPY ONLY THOSE CHARACTERS
                                ;AND BLANK FILL VECTOR
HOLL:   MOVE    10,BLANK        ;PRESET WORD TO BLANKS
        MOVE    12,NCH          ;DETERMINE NUMBER OF WORDS
        IDIV    12,J            ;IN HOLLERITH STRING
        CAIE    13,0            ;REST TERM
        ADDI    12,1
LOOP:   MOVEM   10,(5)
        ADDI    5,1
        SOJG    12,LOOP
        MOVE    12,WRD          ;GET ILDB POINTER FOR HOLLERITH STRING
COPY:   ILDB    11,1            ;GET NEXT CHARACTER
        IDPB    11,12           ;STORE IT
        SOJG    6,NEXT          ;GET RIGHT NUMBER OF CHARACTERS/WORD
        MOVE    6,J             ;RESET J
        MOVE    12,WRD          ;RESET POINTER
        AOS     VJ              ;FOR NEXT WORD
NEXT:   SOJG    7,COPY          ;COUNT THE NUMBER OF CHAR./WORD
END:    POPJ    17,0
 
 ;      VARIABLES
 
VJ:     0
NCH:    0
J:      0
BLANK:  ASCII/     /
WRD:    440720,,VJ
PRGEND
#ifdef CERNLIB_TCGEN_UCTOH
#undef CERNLIB_TCGEN_UCTOH
#endif
