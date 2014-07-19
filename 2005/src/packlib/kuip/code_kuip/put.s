*
* $Id: put.s,v 1.1.1.1 1996/03/08 15:32:59 mclareni Exp $
*
* $Log: put.s,v $
* Revision 1.1.1.1  1996/03/08 15:32:59  mclareni
* Kuip
*
*
#if defined(CERNLIB_IBMMVS)
*CMZ :  2.05/02 11/05/94  09.50.52  by  Alfred Nathaniel
*-- Author :
*   27/06/90 006291027  MEMBER NAME  $PUT     (XCMZ.S)      ASSEMBLER
*
***********************************************************************
*          $PUT PROGRAM                                               *
*              PUT A MESSAGE ON A TSO TERMINAL                        *
*                                                                     *
*         AUTHOR:                                                     *
*              H.-P. SOMMER   DESY (DHHDESY3@R02SOM) MVS(XA),RMODE=ANY*
*                                                                     *
*         CALLING SEQUENCE:                                           *
*                   CALL $PUT ( OUT, LGOUT, IST )                     *
*                                                                     *
*         OUT       OUTPUT CHARACTER STRING                 (INPUT)   *
*         LGOUT     NB OF CHARACTERS OF OUT                 (INPUT)   *
*         IST       STATUS                                  (OUTPUT)  *
*              -01  BAD LGOUT                                         *
*              -08  'ATTN' INTERRUPT RECEIVED                         *
*              -12  BUFFER TOO SMALL                                  *
*              -20  TERMINAL DISCONNECTED                             *
*                                                                     *
***********************************************************************
*                                                                     *
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
$PUT     AMODE     31
$PUT     RMODE     ANY
*
$PUT     START     0
         REGS
         USING     *,R12
         SAVE      (14,12)
         LR        R12,R15
         LM        R2,R4,0(R1)
         LR        R11,R4
         GETMAIN   R,LV=DYNL
         ST        R13,4(R1)
         ST        R1,8(R13)
         LR        R13,R1
         USING     WORK,R13
*
         L         R3,0(R3)                LGOUT
         LA        R15,1                   STATUT SI LGOUT MAUVAIS
         LTR       R3,R3
         BNH       ERREUR
         CH        R3,LGMAX
         BNL       ERREUR
*
         LR        R8,R2              EXTERN TPUT STRING            @31
         LR        R7,R3              LENGTH OF STRING              @31
         LA        R6,TEXTSTR         ADDRESS OF LOCAL STRING BELOW 16M
         LR        R9,R7              SAME LENGTH TO SECOND OPERAND @31
         MVCL      R6,R8              MOVE STRING BELOW 16MB-LINE   @31
*
         LA        R6,TEXTSTR         ADDRESS OF LOCAL STRING BELOW 16M
*                                     ADD NEWLINE CHARACTER AT THE END
         LR        R8,R6
         AR        R8,R3
         MVI       0(R8),X'15'        NL RETURNS CURSOR
         LA        R3,1(R3)
*  TPUT   -  WITH OPTIONS ASIS AND LINE FEED
         TPUT      (6),(3),ASIS
*
ERREUR   LCR       R15,R15
         ST        R15,0(R11)
         L         R3,4(R13)
         FREEMAIN  R,LV=DYNL,A=(R13)
         LR        R13,R3
         RETURN    (14,12),T,RC=0
*
LGMAX    DC        H'132'
*
WORK     DSECT
SAVE     DS        18F             72 BYTES FOR SAVE AREA REQUIRED
TEXTSTR  DS        CL(132+1)       MAX TEXT PLUS NL
DYNL     EQU       *-SAVE
         END
#endif
