;
; $Id: getbitvx.s,v 1.1.1.1 1996/02/15 17:47:50 mclareni Exp $
;
; $Log: getbitvx.s,v $
; Revision 1.1.1.1  1996/02/15 17:47:50  mclareni
; Kernlib
;
;
 .TITLE  GETBIT
;       SUBROUTINE SETBIT:      SET THE VALUE OF A BIT IN A BIT STRING
;
;
SETBIT::
        .WORD  ^M<R2,R3>
        MOVL   @4(AP),R0               ;I
        MOVAL   @8(AP),R2               ;M
        MOVAL   @12(AP),R3
        DECL    R0
        EXTZV   #0,#5,R0,R1
        MCOML    R1,R1
        INSV    R1,#0,#5,R0
        INSV    (R3),R0,#1,(R2)
        RET
;
;       SUBROUTINE GETBIT:       GET THE VALUE OF A BIT IN
;                                A BIT STRING
;
GETBIT::
        .WORD  ^M<R2,R3>
        MOVL   @4(AP),R0               ;I
        MOVAL   @8(AP),R2               ;M
        MOVAL   @12(AP),R3
        DECL    R0
        EXTZV   #0,#5,R0,R1
        MCOML    R1,R1
        INSV    R1,#0,#5,R0
        BBS     R0,(R2),BSET
        BICL2   #1,(R3)
        RET
BSET:   BISL2   #1,(R3)
        RET
        .END
