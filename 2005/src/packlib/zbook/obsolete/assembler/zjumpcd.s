*
* $Id: zjumpcd.s,v 1.1.1.1 1996/03/08 12:01:16 mclareni Exp $
*
* $Log: zjumpcd.s,v $
* Revision 1.1.1.1  1996/03/08 12:01:16  mclareni
* Zbook
*
*
#if defined(CERNLIB_CDC)
          IDENT     ZJUMP
          TITLE     ZJUMP - CALL TO ZBOOK RECOVERY ROUTINE
          COMMENT           CALL TO ZBOOK RECOVERY ROUTINE
*****
*
*     CALLING SEQUENCE - CALL ZJUMP(ADR,P1,P2,.....,PN)
*
*     ROUTINE WILL PERFORM RETURN JUMP TO THE RECOVERY ROUTINE
*     WITH THE ENTRY POINT ADDRESS GIVEN IN THE 1-ST PARAMETER (ADR).
*     THE PARAMETERS  (P1,...,PN)  WILL BE TRANSMITED AS THE
*     PARAMETERS TO THE RECOVERY ROUTINE.
*
*     THE PARAMETER ADR MUST BE CALCULATED USING THE CALL
*     TO THE FUNCTION LOCF IN THE CALLING ROUTINE
*     (RECOVER MUST BE DECLARED EXTERNAL).
*
*        ADR = LOCF(RECOVER)
*
*     IN THE CALLING ROUTINE AND RECOVER MUST BE DECLARED EXTERNAL.
*
*     FTN4 CALLING CONVENTIONS
*
*     W.W.  20/05/81
*
*****
          ENTRY  ZJUMP
 TRACE    VFD    42/0HZJUMP,18/ZJUMP
 ZJUMP    EQ     *+1S17       .ENTRY/EXIT
*                             .ASSUMING FTN4 CONVENTIONS,
*                             .ADDRESS OF THE 1-ST PARAM. IN X1
          SA2    X1           .GET ADDRESS OF THE 1-ST PARAM.
          SA5    CALL         .LOAD RJ INSTRUCTION INTO THE X5
          SX0    X2           .GET BITS 0-17 OF THE 1-ST PARAM. INTO X0
          MX7    -18          .FORM MASK IN X7
          LX0    30           .SHIFT ADDRESS IN X0
          LX7    30           .SHIFT MASK IN X7
          BX4    X7*X5        .FORCE ZERO INTO ADDRESS PART OF RJ
          SB1    1            .B1=1
          BX6    X4+X0        .PUT THE ADDRESS FROM X0 INTO RJ
          SA1    A1+B1        .PREPARE ADDRESS OF PARAM. LIST
          SA6    CALL         .SAVE RJ INTO HIS PLACE
 +        RJ     TRICK        .THIS IS THE TRICKY CALL,
*                             .FOR CLEARING THE STACK.
 TRICK    BSSZ   1
 CALL     RJ     CALL         .THIS WILL DO THE JOB......
 -        VFD    12/0,18/TRACE
          EQ     ZJUMP        .THE JOB IS DONE, RETURN......
          END
#endif
