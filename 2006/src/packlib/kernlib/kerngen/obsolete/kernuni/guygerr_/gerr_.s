*
* $Id: gerr_.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: gerr_.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. ---------------------------------------
.        UTILITY AND ERROR ROUTINES
. 1   COMMON/ G$DAT$/ LOAD-TIME PARAMETERS
. 2   SCRATCH-PAD
. 3 ERROR ROUTINE GERR$ (REFERENCED BY GNSEC LIBRARY ROUTINES)
. 4   DEVICE CHECK  ROUTINE
. 5   CONVERSION ROUTINES
.        THIS PACKAGE IS LOADED IN  D BANK
. ---------------------------------------
 
          AXR$
          FORT$
/
. -----------------------------------------------------------------------
.     AREA  1.0
.     THIS IS A COMMON WHICH CONTAINS ALL VARIABLES OF INTEREST FOR GERR$
.     AND ROUTINES OUTSIDE THIS PACKAGE DEALING WITH SYSTEM DEPENDENT PARAMETERS
.     (FOR INSTANCE VALUES SET BY @MAP AT LOAD TIME)
 
G$DAT$    INFO 2 2
$(2)
.         HYPOTHESIS:ONLY ONE I-BANK,ONLY PSRM USED
IBKL      + FRSTI$-1
          + LASTI$
DBKL      +FRSTD$-1
          + LASTD$
 
. ----------------------------------------------------------------------
.     AREA 2 :
.     SCRATCH-PAD  (COMMMON TO ALL ROUTINES OF THE PACKAGE)
.     THE COMMON  GERR$C  CONTAINS  DYN$  (=SAVX11) FOR ALL ASSEMBLER
.     ROUTINES IN GNSECLIB
.     AND SOME HEAVILY USED CONSTANTS
GERR$C    INFO  2  4
$(4)
 DO FOR ,FBLANK
BLANK
 DO FOR , +         050505050505       . SPACE, FIELDATA
 DO FTN , +         040040040040       . SPACE, ASCII
DYN$
SAVX11
          +         0
          +         0
WKB      . WALK-BACK WORD FOR GERR$
. FACPKT*  . 13 WORDS PACKET USED BY DCHECK
          RES       1
. BSAV1    . 2 WORDS USED BY CONVERSION ROUTINE BNOCT$
.           RES       12
 
. ----------------------------------------------------------------------
.     AREA 3.0 :
.     LOCAL VARIABLES FOR ERROR ROUTINE GERR$
 
MXARG     EQU       6
          ON    FORT**DIAGP
F         FORM      6,6,6,18
          INFO      010 5
$(5)
DIAG0     F         077,060,3,GERR$
          +         WKB
          +         'GERR$ '
DIAG1     F         077,062,2,LMJSTR+1
          +         LMJSTR-FIRS00,DIAG0
DIAG2     F         077,067,2,LMJNER+1
          +         WKB
DIAG3     F         077,067,2,LMJNR1+1
          +         WKB
          OFF
 
PF        FORM      12,6,18
 
$(0),FIRS00        . LABEL  FIRS00  USED TO TO GET RELATIVE ADDRESS INSIDE GERR$
.                            . ---- AREA RESERVED TO STORE REGISTERS
SAVREG    RES       9
.                            . ----
#include "flagwd.inc"
FLAGF1    EQUF      FLAGW1,,S6
FLAGF2    EQUF      FLAGW1,,S1
FLAGF3    EQUF      FLAGW1,,S2
 DO FTN ,FBLANK     '      '          . FIELDATA BLANK WORD
MSG1      '***** ERROR IN ROUTINE              '
LMSG1     EQU       $-MSG1
PRPKT1    PF        1,LMSG1,MSG1
MSG2     'GERR$: ROUTINE NAME CANNOT BE ACCESSED'
LMSG2     EQU       $-MSG2
PRPKT2    PF        1,LMSG2,MSG2
          ON    FORT**DIAGP
INOP      NOP
BTREG     +         A3
          +         1,LMJSTR+1
          OFF  .  FORT**DIAGP
 
.     AREA 3.1 :
.     CODE FOR ERROR ROUTINE GERR$
 
$(0)
. ENTRY THRU  J  OR  LMJ
. GERRR$*
.         SLJ       SVREG
.         S,H2      X11,WKB
.         J         GER0
. ENTRY THRU   SLJ
GERR$*
          NOP
          SLJ       SVREG             . SAVE THE REGISTERS WHICH MAY BE USED BY
          L,H2      A2,GERR$           . LOC( ERROR DETECTION )
           S        A2,WKB             . WALK-BACK WORD FOR DIAG PACKET
GER0
          LN,U      A0,1
          DL        A2,IBKL
          SN        A0,FLAGF1
          DL        A4,DBKL
          A,H2      A0,SAVX11          . (A0)=(X11)-1
          TNW       A2,A0              .
          J         $+3                . (X11) WITHIN I-BANK
          TW        A4,A0              .
          J         GER6               . (X11) NOT WITHIN PROGRAM LIMITS
          LXI,U     A0,-1              .
GER1      L,H1      A1,0,*A0           . . LOOP USED TO FIND LMJ INSTRUCTION
 DO FOR , JZ        A1,GER1           . .
 DO FTN , JZ        A1,GER6           . .
          TE,U      A1,0745660         .
          J         GER6               . NOT A LMJ X11
          L,H2      A1,1,A0            . LOC ( EBTRY POINT OF ROUTINE CALLED )
          A,U       A0,2
          S,H2      A0,SAVX11          . UPDATE SAVX11 VALUE
                                       . ( CORRECT KEY FOR DIAG PACKET )
          TG,U      A1,0200000         . CHECK NO INDIRECTION
          J         GER6               .
          TNW       A2,A1
          J         $+3                . WITHIN I-BANK LIMITS
          TW        A4,A1
          J         GER6
          AN,U      A1,1
          L         A4,0,A1            . ROUTINE NAME OR LOC(ROUTINE NAME)
          L         A5,FBLANK
          TNZ,H1    0,A1
          DL        A4,*0,A1
          DS        A4,MSG1+4
          L         A0,PRPKT1
          ER        PRINT$
          ON    FOR
          SLJ       N$ARG              . TO GET THE NUMBER OF ARGUMENTS
          S,H2      A0,GER5            . STORE IT IN RETURN INSTRUCTION
          OFF . FOR
          ON    FORT**DIAGP
          TNZ       FLAGF2
          J         GER2               . NO STRACE
          DL        A1,BTREG           . . SETS THE LIST OF ARGUMENTS
          L         A3,INOP            . . OF STRACE TO 'NOP'
          L,U       R1,MXARG           . .
          BT        A2,0,*A1
          JZ        A0,LMJSTR          . IF THERE ARE NO ARGUMENTS
          TG,U      A0,MXARG+1         .
          L,U       A0,MXARG           . CONTROL: NO MORE THAN MXARG ARGUMENTS
          LXI,U     X11,1              . . THE ARGUMENT LIST OF THE (USER)
          S         A0,R1              . . CALLING ROUTINE IS TRANSFERED
          LXM,U     A2,LMJSTR+1        . . INTO STRACE ARGUMENT LIST.
          BT        A2,0,*X11          . .
.         SLJ  LDREG  NOT NEEDED SINCE STRACE NOW RETURNS IN THE CALLING ROUTINE
.  NOT IN THE FIRST UPSTREAM USER ROUTINE AS BEFORE (1976)
LMJSTR    LMJ       X11,STRACE         .
          RES       MXARG              .
          OFF  .  FORT**DIAGP
GER2
          SLJ       LDREG
          TZ        FLAGF3
 DO FORE ,LMJNER  LMJ  X11,NERR$
 DO FORT ,LMJNER  LMJ  X11,NER$
 DO FTN  ,LMJNER  LMJ  X11,FERR
 
.     CONTROL RETURNED HERE IF  ('FERRER') ERROR RECOVERY TURNED ON
          L,H2      X11,SAVX11
.  RETURN IF  J  OR  SLJ  OR  LMJ
GER5      J         $-$,X11            . RETURN IF  LMJ
 
GER6                                   . INCORRECT ADDRESSING.
          SLJ       LDREG
          ON    FORT
LMJNR1    LMJ       X11,NER1$
          +         PRPKT2
. RETURN FROM  NER1$  IF ANY IS TO FIRST USER ROUTINE (DIAG PACKET 060)
          OFF  .  FORT
 DO FORE  ,  J  NERR$
 DO FORT , J        NER$
 DO FTN , J         FERR
 
SVREG     NOP       +0
          DS        A0,SAVREG
          S,H2      X11,SAVX11
          S         R1,SAVREG+6
          DS        A2,SAVREG+2
          S         R2,SAVREG+7
          S         R3,SAVREG+8
          DS        A4,SAVREG+4
          J         *SVREG
 
LDREG     NOP       +0
          DL        A0,SAVREG
          L,H2      X11,SAVX11
          L         R1,SAVREG+6
          DL        A2,SAVREG+2
          L         R2,SAVREG+7
          L         R3,SAVREG+8
          DL        A4,SAVREG+4
          J         *LDREG
 
. ---------------------------------------
.         AREA      3.11
.         CODE FOR ROUTINES LIBCHK AND SETDBG
.
. LOGICAL FUNCTION LIBCHK ( DUMMY ARG )
.         TEST AND RESET OF ERROR FLAG FOR LIBRARY ROUTINES
.         LIBCHK=.FALSE. IF NO ERROR DETECTED SINCE LAST CALL TO LIBCHK
.     LIBCHK = .TRUE.   IF ONE ERROR AT LEAST DETECTED
.
LIBCHK*
          L         A0,FLAGF1
          SZ        FLAGF1             . RESETS FLAG
          J         RETUR1
.
. SUBROUTINE SETDBG(LEVEL1,...)
.         SET OR RESET DEBUG FLAG FOR LIBRARY ROUTINES
.         LEVEL=0:  RESETS ALL DEBUG OPTIONS ( IF ANY )
.         LEVEL=1:  SWITCH-ON WALK BACK AND DUMP OF ARGUMENTS
.         LEVEL=2:  JUMP TO STANDARD ERROR ROUTINE NER$.
.
SETDBG*
#include "ftnors1.inc"
          L,U       A2,1
          ON    FTN
          L         A4,XARG
          SSL       A4,18
          OFF . FTN
          LXI,U     XARG,1
 DO FOR ,SET1  TZ,S1  0,XARG
 DO FTN ,SET1  JGD    A4,SET12
          J         0,X11              . RETURN IF ARGUMENT LIST EXHAUSTED
SET12
          L         A1,0,*XARG
          L         A1,0,A1           . NEXT ARGUMENT
          TE,U      A1,1
          J         SET2
          S         A2,FLAGF2          . LEVEL=1
          J         SET1
SET2
          TE,U      A1,2
          J         SET3
          S         A2,FLAGF3          . LEVEL=2
          J         SET1
SET3
          JNZ       A1,SET1
          SZ        FLAGF2             . LEVEL=0
          SZ        FLAGF3
          J         SET1
. ----------------------------------------------------------------------
 
. 3.2
.     CODE FOR ROUTINE  N$ARG  (SIMILAR TO LIBRARY ROUTINE 'NOARG')
. RETURNS IN  A0  THE NUMBER WHICH MUST BE ADDAD TO  X11  FOR CORRECT RETURN
.     ( 'NUMBER OF ARGUMENTS' )
N$ARG*
          NOP                          . ENTRY IS THRU  SLJ
#if !defined(CERNLIB_GUYFTN)
          L         A0,X11             . SET LOOP INDEX
          LXI,U     A0,1               . LOOP INCREMENT
N1
          L,H1      A2,0,*A0           . H1 OF NEXT WORD IN ARGUMENT STRING
          JZ        A2,N1              .
          TNE,U      A2,0743000        . IS IT A 'NOP'
          J         N3                 . YES
          TLE,U     A2,010000          . IS IT A W.B. WORD (11A COMPILER)
          J         N4                 . YES
          TE,U      A2,0742000         . IS IT A 'J' INSTRUCTION
          J         N3                 . NO (CONSIDER IT AS A VALID RETURN POINT
. NOW HAVE TO DECIDE IF 'J'  WAS THE NORMAL RETURN, OR A $NNN ARGUMENT
          L         A1,A0              . SAVE PRESENT  A0  VALUE
N2
          L,H1      A2,0,*A0           . H1 OF NEXT WORD
          JZ        A2,N1              . RETURN TO NORMAL SEQUENCE IF ZERO
          TNE,U     A2,0743000         . IS IT A  'NOP'
          J         N3                 . YSE
          TLE,U     A2,010000          . IS IT A  W.B.  WORD
          J         N4                 . YES
          TNE,U      A2,0742000        . IS IT A  'J'
          J         N2                 . YES, LOOK FOR NEXT INSTRUCTION
          L         A0,A1              . ANOTHER INSTRUCTION, CONSIDER THE
                             . FIRST JUMP INSTRUCTION AS THE NORMAL RETURN POINT
N3        AN,U      A0,1               . TO GET THE CORRECT NUMBER OF ARGS
N4        ANH       A0,X11
          LXI,U     A0,0
#endif
#if defined(CERNLIB_GUYFTN)
          L,U       A0,0
#endif
          J         *N$ARG             . AND RETURN
 
. ---------------------------------------
. 3.3    FUNCTION  VAL( X )     USE RELATED TO NOARG
 
VAL*
IVAL*
LVAL*
#include "ftnors1.inc"
          L         A0,0,XARG
          L         A0,0,A0
          J         RETUR1
CVAL*
DVAL*
#include "ftnors1.inc"
          L         A0,0,XARG
          DL        A0,0,A0
          J         RETUR1
 
          END
