*
* $Id: comisass.s,v 1.1.1.1 1996/02/26 17:16:46 mclareni Exp $
*
* $Log: comisass.s,v $
* Revision 1.1.1.1  1996/02/26 17:16:46  mclareni
* Comis
*
*
*CMZ :  1.19/00 03/04/95  11.34.01  by  Unknown
*-- Author :
      MODULE    JUMPS
      ENTRY.P   CSBCLL
      ENTRY.P   CIFNCL
      ENTRY.P   CLFCLL
*******************************************************************
*                                                                 *
*       SUBROUTINE CSBCLL(J,P1,P2,...)                             *
*                                                                 *
*       FUNCTION   CIFNCL(J,P1,P2,...)                            *
*                                                                 *
*       FUNCTION   CLFCLL(J,P1,P2,...)                            *
*                                                                 *
*******************************************************************
        PROC
*
*       Entry prologue
*
PROC1_BEG  EQU   *
      PEA       (DB)
      CLR.L     -(SP)
      PEA       (A0)
      MOVEA.L   6(A0),DB
      LINK      SB,#0
*
*       Calculate number of arguments (bytes!!)
*
      MOVE.L    SB,D1             Get address of SB into D1
      MOVE.L    (SB),D2           Get address of calling routine in D2
      SUB.L     D1,D2             Compute the distance between the two SB's
      SUBI.L    #20,D2            Substract 20 bytes to get the number of args
      SUB.L     #4,D2             Do not count the routine name
      BEQ       CALL
*
*       Copy the arguments to the top of the stack
*
LOOP  MOVE.L    20(SB,D2.L),-(SP)
      SUB.L     #4,D2
      BGT       LOOP
CALL  MOVEA.L    20(SB),A2
      MOVEA.L    (A2),A2
      MOVEA.L   (A2),A0
      MOVEA.L   4(A2),A2
      JSR       (A0)
*
*       Exit epilogue
*
      UNLK      SB
      ADDQ.W    #8,SP
      MOVEA.L   (SP)+,DB
      RTS
        DATA
*
*        ECB for all routines
*
CSBCLL  JMP      PROC1_BEG
       AC       0
       DC.W     0
CIFNCL JMP      PROC1_BEG
       AC       0
       DC.W     0
CLFCLL JMP      PROC1_BEG
       AC       0
       DC.W     0
     ENTRY.P   CSCHAR
*      CALL CSCHAR(J,1H*)
        PROC
*
*       Entry prologue
*
PROC_BEG  EQU   *
      PEA       (DB)
      CLR.L     -(SP)
      PEA       (A0)
      MOVEA.L   6(A0),DB
      LINK      SB,#0
      MOVEA.L   20(SB),A2
      MOVEA.L   (A2),A2
      MOVEA.L   24(SB),A3
      MOVE.B    (A3),D2
      MOVE.B    D2,(A2)
*
*       Exit epilogue
*
      UNLK      SB
      ADDQ.W    #8,SP
      MOVEA.L   (SP)+,DB
      RTS
        DATA
*
*        ECB for  routine
*
CSCHAR  JMP      PROC_BEG
       AC       0
       DC.W     0
     ENTRY.P   MKCHAR
*      K=MKCHAR(J)
        PROC
*
*       Entry prologue
*
PROC_BEG1  EQU   *
      PEA       (DB)
      CLR.L     -(SP)
      PEA       (A0)
      MOVEA.L   6(A0),DB
      LINK      SB,#0
      MOVEA.L   20(SB),A2
      MOVEA.L   (A2),A2
      MOVE.L    #538976288,D0
      MOVE.B    (A2),D0
      ROR.L     #8,D0
*
*       Exit epilogue
*
      UNLK      SB
      ADDQ.W    #8,SP
      MOVEA.L   (SP)+,DB
      RTS
        DATA
*
*        ECB for  routine
*
MKCHAR  JMP      PROC_BEG1
       AC       0
       DC.W     0
      ENTRY.P   CSLCAL
      ENTRY.P   CSCALI
      ENTRY.P   CSCALR
      ENTRY.P   CSCALD
*******************************************************************
*                                                                 *
*       SUBROUTINE CSLCAL(J,NPAR,M)                               *
*                                                                 *
*       FUNCTION   CSCALI(J,NPAR,M)                               *
*                                                                 *
*       FUNCTION   CSCALR(J,NPAR,M)                               *
*                                                                 *
*       FUNCTION   CSCALD(J,NPAR,M)                               *
*******************************************************************
        PROC
*
*       Entry prologue
*
PROC2_BEG  EQU   *
      PEA       (DB)
      CLR.L     -(SP)
      PEA       (A0)
      MOVEA.L   6(A0),DB
      LINK      SB,#0
      MOVEA.L   24(SB),A3
      MOVEA.L   (A3),D2
      BEQ       CALL1
      MOVEA.L   28(SB),A3
      LSL.L     #2,D2
*      D2*4
LOOP1 MOVE.L    -4(A3,D2.L),-(SP)
      SUB.L     #4,D2
      BGT       LOOP1
CALL1 MOVEA.L    20(SB),A2
      MOVEA.L    (A2),A2
      MOVEA.L   (A2),A0
      MOVEA.L   4(A2),A2
      JSR       (A2)
*
*       Exit epilogue
*
      UNLK      SB
      ADDQ.W    #8,SP
      MOVEA.L   (SP)+,DB
      RTS
        DATA
*
*        ECB for all routines
*
CSLCAL  JMP      PROC2_BEG
        AC       0
        DC.W     0
CSCALI  JMP      PROC2_BEG
        AC       0
        DC.W     0
CSCALR  JMP      PROC2_BEG
        AC       0
        DC.W     0
CSCALD  JMP      PROC2_BEG
        AC       0
        DC.W     0
