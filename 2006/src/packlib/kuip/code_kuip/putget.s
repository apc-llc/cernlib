*
* $Id: putget.s,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: putget.s,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#if defined(CERNLIB_IBMMVS)
*CMZ :  2.05/02 11/05/94  09.50.52  by  Alfred Nathaniel
*-- Author :
***********************************************************************
*         $PUTGET PROGRAM                                             *
*             PROMPT A MESSAGE ON A TSO TERMINAL AND READ TYPED ANSWER*
*             ON THE SAME LINE                                        *
*                                                                     *
*         AUTHOR:                                                     *
*              G. WEIL      CENTRE DE CALCUL DE STRASBOURG (CNRS)     *
*         CHANGES 20/06/90.   MVS/XA VALID UNDER: AMODE=31,RMODE=ANY  *
*              H.-P. SOMMER   DESY (DHHDESY3@R02SOM)                  *
*                                                                     *
*         CALLING SEQUENCE:                                           *
*                   CALL $PUTGET ( OUT, LGOUT, IN, LGIN, IST )        *
*                                                                     *
*         OUT       OUTPUT CHARACTER STRING (PROMPT)        (INPUT)   *
*         LGOUT     NB OF CHARACTERS OF OUT                 (INPUT)   *
*         IN        READ CHARACTER STRING                   (OUTPUT)  *
*         LGIN      CHARACTER MAX SIZE OF IN                (INPUT)   *
*         IST       STATUS                                  (OUTPUT)  *
*               >0  NB OF CHARACTERS READ                             *
*              -01  BAD LGOUT                                         *
*              -02  BAD LGIN                                          *
*              -08  'ATTN' INTERRUPT RECEIVED                         *
*              -12  BUFFER TOO SMALL                                  *
*              -20  TERMINAL DISCONNECTED                             *
*                                                                     *
***********************************************************************
*
*         MACRO REGS
          MACRO
          REGS
          PUSH      PRINT
          PRINT     ON,NOGEN
R0        EQU       0
R1        EQU       1
R2        EQU       2
R3        EQU       3
R4        EQU       4
R5        EQU       5
R6        EQU       6
R7        EQU       7
R8        EQU       8
R9        EQU       9
R10       EQU       10
R11       EQU       11
R12       EQU       12
R13       EQU       13
R14       EQU       14
R15       EQU       15
F0        EQU       0
F2        EQU       2
F4        EQU       4
F6        EQU       6
          POP       PRINT
          MEND
*
*
$PUTGET   AMODE     31
$PUTGET   RMODE     ANY
*
$PUTGET   START     0
PUTFLG    EQU       B'00000001'
GETFLG    EQU       B'10000000'
         REGS
         USING     *,R12
         SAVE      (14,12)
         LR        R12,R15
         LM        R2,R6,0(R1)
         LR        R11,R6
         GETMAIN   R,LV=72+132
         ST        R13,4(R1)
         ST        R1,8(R13)
         LR        R13,R1
         USING     WORK,R13
*
         L         R3,0(R3)                LGOUT
         L         R5,0(R5)                LGIN
         LA        R15,1                   STATUT SI LGOUT MAUVAIS
         LTR       R3,R3
         BNH       ERREUR
         CH        R3,LGMAX
         BNL       ERREUR
         LA        R15,2                   STATUT SI LGIN MAUVAIS
         LTR       R5,R5
         BNH       ERREUR
         CH        R5,LGMAX
         BNL       ERREUR
* TPUT
         LR        R8,R2              EXTERN TPUT STRING            @31
         LR        R7,R3              LENGTH OF STRING              @31
         LA        R6,TEXTSTR         ADDRESS OF LOCAL STRING BELOW 16M
         LR        R9,R7              SAME LENGTH TO SECOND OPERAND @31
         MVCL      R6,R8              MOVE STRING BELOW 16MB-LINE   @31
*
         LA        R6,TEXTSTR         ADDRESS OF LOCAL STRING BELOW 16M
         LA        R7,PUTFLG          FLAGS POUT TPUT
         SLL       R7,24
         OR        R6,R7              OUTPUT STRING
         TPUT      (6),(3),R
         LTR       R15,R15
         BNZ       ERREUR
* TGET
         LA        R6,TEXTSTR         ADDRESS OF LOCAL STRING BELOW 16M
         LA        R8,GETFLG          FLAGS POUR TGET
         SLL       R8,24
         OR        R6,R8              INPUT  STRING
         TGET      (6),(5),R
         LTR       R15,R15
         BNZ       ERREUR
         LR        R2,R1              LENGTH OF STRING              @31
         LA        R6,TEXTSTR         ADDRESS OF LOCAL STRING BELOW 16M
         OR        R6,R7              OUTPUT STRING
* DEBUG  TPUT      (6),(5),R
         LR        R6,R4              EXTERN GET STRING             @31
         LR        R7,R2              LENGTH OF STRING              @31
         LA        R8,TEXTSTR         ADDRESS OF LOCAL STRING BELOW 16M
         LR        R9,R7              SAME LENGTH TO SECOND OPERAND @31
         MVCL      R6,R8              MOVE STRING TO TARGET LINE    @31
         LNR       R15,R2
*
*
ERREUR    LCR       R15,R15
          ST        R15,0(R11)
          L         R3,4(R13)
          FREEMAIN  R,LV=72+132,A=(R13)
          LR        R13,R3
          RETURN    (14,12),T,RC=0
LGMAX     DC        H'132'
*
WORK     DSECT
SAVE     DS        18F             72 BYTES FOR SAVE AREA REQUIRED
TEXTSTR  DS        CL132
DYNL     EQU       *-SAVE
          END
#endif
