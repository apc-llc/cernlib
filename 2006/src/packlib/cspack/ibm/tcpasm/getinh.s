*
* $Id: getinh.s,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
*
* $Log: getinh.s,v $
* Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
* Cspack
*
*
#if defined(CERNLIB_IBMMVS)
*         $GETINH (PL/I)
*          GETINH (FORTRAN,PASCAL)
*
*        PROMPT A MESSAGE ON A TSO TERMINAL AND READ TYPED ANSWER
*        ON THE SAME LINE
*
*         AUTHOR:
*              G. WEIL      CENTRE DE CALCUL DE STRASBOURG (CNRS)
*
*        17/02/91: /MK/,GSI
*                  MODIFIED TO RUN IN AMODE=31,RMODE=ANY.
*        18/02/91: /MK/,GSI
*                  DO NOT ECHO USER RESPONSE (E.G. FOR PASSWORDS)
*        19/02/91: /MK/,GSI
*                  SEPARATE ENTRIES FOR FORTRAN/PASCAL AND PL/I
*
*        PL/I USAGE:
*        ==============
*
*        DCL OUT,IN CHAR(*) VAR;  ! PROMPT & READ STRING
*    OR  DCL OUT,IN CHAR(*);      ! PROMPT & READ STRING
*        BIN FIXED(31) IST;       ! RETURN CODE
*
*        CALL $GETINH( OUT, IN, IST );
*
*        FORTRAN USAGE:
*        ==============
*
*        CHARACTER*(*) OUT, IN    ! PROMPT & READ STRING
*        INTEGER     LGOUT, LGIN  ! THEIR MAX. LENGTH
*        INTEGER     IST          ! RETURN CODE
*                   CALL GETINH ( OUT, LGOUT, IN, LGIN, IST )
*
*        PASCAL  USAGE:
*        ==============
*
*       program test;
*       type
*          stringxx =                     packed array [1..xx] of char;
*          Procedure GETINH( var  cbufin: stringxx;
*                            var  lbufin: integer;
*                            var cbufout: stringxx;
*                            var lbufout: integer;
*                            var irc: integer );
*          external;
*       var
*          cprompt : packed array [1..80] of char;
*          cc      : packed array [1..80] of char;
*          lin, lout, nn : integer;
*
*       begin;
*          cprompt:='enter password 1';
*          lin := 16; lout := 80; nn := 0;
*          GETINH( cprompt, lin, cc, lout, nn );
*       end.
*
*         IST       STATUS                                  (OUTPUT)
*               >0  NB OF CHARACTERS READ
*              -01  BAD LGOUT
*              -02  BAD LGIN
*              -08  'ATTN' INTERRUPT RECEIVED
*              -12  BUFFER TOO SMALL
*              -20  TERMINAL DISCONNECTED
*
*
*
$GETINH   AMODE     31
$GETINH   RMODE     ANY
*
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
$GETINH  CSECT                    PL/I ENTRY
         ENTRY GETINH
          REGS
         USING $GETINH,R15        addressability
          SAVE      (14,12)       SAVE REGISTERS
          LM        R2,R4,0(R1)   LOAD PL/I PARAMETER ADDRESSES
         LR    R6,R4              SAME REG.S AS FORTRAN
         LR    R4,R3
         SR    R7,R7              RESET PTR. TO VAR. STRING PREFIX
*
*                      PL/I STRING HANDLING, PROMPT
*
         LA    R3,4(0,R2)         PTR. TO MAX LENGTH
         L     R2,0(0,R2)         PTR. TO STRING ITSELF
         TM    2(R3),X'80'        FIXED/VARYING ?
         BZ    OUT_FIXED
            LA R3,0(0,R2)         PTR. TO PREFIX
            LA R2,2(0,R2)         PTR. TO STRING
OUT_FIXED EQU  *
         LH    R3,0(0,R3)         LOAD STRING LENGTH
*
*                      PL/I STRING HANDLING, input buffer
*
         LA    R5,4(0,R4)         PTR. TO MAX LENGTH
         L     R4,0(0,R4)         PTR. TO STRING
         TM    2(R5),X'80'        FIXED/VARYING ?
         BZ     IN_FIXED
            LA R7,0(0,R4)         PTR. TO PREFIX
            SR R0,R0
            STH R0,0(0,R7)        RETURN NULL STRING (DEFAULT)
            LA R4,2(0,R4)         PTR. TO STRING
IN_FIXED EQU  *
         LH    R5,0(0,R5)         LOAD MAX. STRING LENGTH
         LA    BASE,$GETINH_START ADDRESSABILITY
         DROP  R15                GIVE UP
         USING $GETINH_START,BASE SAME BASE AS FORTRAN ENTRY
         B     $GETINH_START
*
* ***
*
GETINH    EQU       *             FORTRAN & PASCAL ENTRY
*
         DROP  BASE
         USING  GETINH,R15        ADDRESSABILITY
          SAVE      (14,12)
          LM        R2,R6,0(R1)   PICKUP PARAMETER ADDRESSES
          L         R3,0(0,R3)    LENGTH PROMPT STRING
          L         R5,0(0,R5)    LENGTH BUFFER STRING
         SR    R7,R7
         LA    BASE,$GETINH_START
         DROP  R15
         USING $GETINH_START,BASE
$GETINH_START  EQU  *             COMMON START & BASE REFERENCE
*
* *** CHECK REASONABLE LENGTH OF GIVEN I/O BUFFERS
*
         SR    R15,R15
         BCTR  R15,0              -1
          LTR       R3,R3
          BNH       GO_HOME
         BCTR  R15,0              -2
          LTR       R5,R5
          BNH       GO_HOME
*
* *** DYNAMIC SAVE AREA FOR TPUT/GET
*
          GETMAIN   RC,LV=72
          ST        R13,4(R1)
          ST        R1,8(R13)
          LR        R13,R1
*
* *** ALLOCATE LOCAL BUFFER SPACE < 16MB FOR TPUT/GET
*
         LR    R11,R5
         GETMAIN    R,LV=(R11)              INPUT BUF
         LR    R10,R1
         LA    R9,1(0,R3)         LENGTH+1
         GETMAIN    R,LV=(R9)             OUTPUT BUF
         LR    R8,R1              ADDRESS OF BUFFER
         LR    R0,R8              ADDRESS OF BUFFER
         LR    R1,R3              LENGTH
         MVCL  R0,R2              COPY OUTPUT BUFFER
         LR    R2,R0
         MVI   0(R2),BYP_INP      INSERT BYPASS/INHIBIT CHARACTER
*
* *** TPUT PROMPT STRING AND TGET ANSWER
*
         ICM   R8,B'1000',=AL1(PUTFLG) INSERT FLAG
          TPUT      (R8),(R9),ASIS
          LTR       R15,R15
          BNZ       ERREUR
         ICM   R10,B'1000',=AL1(GETFLG)
          TGET      (R10),(R11),R
          LTR       R15,R15
          BNZ       ERREUR
          LNR       R15,R1
*
* *** COPY BACK THE DATA READ
*
         LR    R0,R10             BUFFER ADDRESS
         ICM   R1,B'1000',=CL1' '    PAD WITH BLANKS
         MVCL  R4,R0              COPY BACK RECEIVED DATA
*
* *** ERROR HANDLING AND CLEANUP
*
ERREUR   LCR       R15,R15
         ST        R15,0(R6)
         LTR   R7,R7
         BE    SKIP_VAR1          RETURN PL/I CHAR(*) VARYING ??
           LTR R15,R15
           BNH SKIP_VAR1
           STH R15,0(0,R7)
SKIP_VAR1 EQU *
         L         R3,4(R13)
         FREEMAIN  RC,LV=72,A=(R13)
         FREEMAIN  R,LV=(R11),A=(R10)
         FREEMAIN  R,LV=(R9),A=(R8)
         LR        R13,R3
         RETURN    (14,12),T,RC=0
*
GO_HOME  EQU   *
         ST    R15,0(R6)
         LTR   R7,R7
         BE    SKIP_VAR2          RETURN PL/I CHAR(*) VARYING ??
           LTR R15,R15
           BNH SKIP_VAR2
           STH R15,0(0,R7)
SKIP_VAR2 EQU *
          RETURN    (14,12),T,RC=0
*
* *** CONSTANT DEFINITIONS
*
PUTFLG    EQU       B'00000001'
GETFLG    EQU       B'10000000'
BYP_INP   EQU       X'24'         BYPASS/INHIBIT CHARACTER
*
BASE     EQU   R12
*
          END
#endif
