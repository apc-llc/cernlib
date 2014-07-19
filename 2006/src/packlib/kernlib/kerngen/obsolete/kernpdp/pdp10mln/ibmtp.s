*
* $Id: ibmtp.s,v 1.1.1.1 1996/02/15 17:53:34 mclareni Exp $
*
* $Log: ibmtp.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:34  mclareni
* Kernlib
*
*
#if defined(CERNLIB_UUOTAPOP)
        TITLE     IBMTP
;#
; CERN PROGLIB#         IBMTP           .VERSION KERNPDP  2.02  840327
; ORIG.  B. KAHLHOFF, RZ UNI KIEL, MAERZ 84
;#
;     ISTAT = IBMTP (UNIT)  SETS INDUSTRY COMPATIBLE MODE FOR UNIT
;                           RETURNS ISTAT=0 IF ALL IS WELL
;                           UNIT = LOGICAL DEVICE NUMBER
 
        P==17
 
        ENTRY     IBMTP
        ENTRY     QPDP9T
 
QPDP9T:
IBMTP:
        MOVE      0,@0(16)      ;GET ARG
        MOVEM     0,UNIT
        PUSH      P,16          ;SAVE AC 16
        MOVEI     16,ARGLST
        PUSHJ     P,TAPOP##
        POP       P,16          ;RESTORE AC 16
        POPJ      P,            ;RETURN TO CALLER
 
        -3,,0
ARGLST: UNIT
        FCODE
        FARG
 
UNIT:   0
FCODE:  2007
FARG:   2
        PRGEND
        TITLE     TAPOP
;#
; CERN PROGLIB#         TAPOP           .VERSION KERNPDP  1.07  840321
; ORIG.  B. KAHLHOFF, RZ UNI KIEL, MAERZ 84
;#
                      ;INTEGER FUNCTION TAPOP(UNIT,FCODE,ARG)
                      ;INTEGER UNIT,FCODE,ARG
 
                      ;UNIT = LOGICAL DEVICE NUMBER
                      ;FCODE = FUNCTION CODE
                      ;ARG = ARGUMENT TO TAPOP.
 
                      ;RETURN 0 IF O.K.
                      ;       TAPOP-BITS ON ERRORS
 
      P==17
 
      ENTRY     TAPOP
 
TAPOP:MOVE      0,@0(16)        ;GET LOGICAL DEVICE NUMBER
      JUMPLE    0,ERROR
      PUSH      P,2             ;SAVE AC 2
      SETZ      2,              ;CLEAR AC 2
L1:   IDIVI     0,12            ;/10
      ADDI      1,20            ;CONVERT TO SIXBIT
      ROTC      1,-6            ;SHIFT INTO AC2
      JUMPN     0,L1
      MOVEM     2,ARGLST+1
      MOVE      1,2             ;GET LOGICAL DEVICE NAME IN SIXBIT
      POP       P,2             ;RESTORE AC2
      SETZ      0,              ;CLEAR AC 0
      DEVNAM    1,              ;GET PHYSICAL DEVICE NAME
        JRST    ERROR           ;ERROR RETURN
      HLLZ      1,1             ;GET GENERIC PART
      CAME      1,[SIXBIT /MTA/]
      JRST      EXIT            ;TAPOP CALL IS DUMMY IF DEVICE IS NOT MTA
      MOVE      1,@1(16)        ;GET PARAMETER FCODE
      MOVEM     1,ARGLST
      MOVE      1,@2(16)        ;GET PARAMETER ARG
      MOVEM     1,ARGLST+2
      MOVE      1,[XWD 3,ARGLST]
      TAPOP.    1,
        MOVE    0,1             ;ERROR RETURN
EXIT: POPJ      P,
 
ERROR:SETO      0,
      JRST      EXIT
 
ARGLST: 0                       ;FUNCTION CODE
        0                       ;SIXBIT DEVICE
        0                       ;ARGUMENT
 
;  ------------  ALTERNATIVE ROUTINE IBMTP BELOW  ----------------
        PRGEND
#endif
