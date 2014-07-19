*
* $Id: whichml.s,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: whichml.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
#if defined(CERNLIB_IBM)
WHICH     CSECT
#if defined(CERNLIB_QMIBMXA)
WHICH    AMODE ANY
WHICH    RMODE ANY
#endif
*
*     MODULE         WHICH
*
*     ENTRY POINT    WHICH
*
*     TYPE           NON-REENTRANT, FORTRAN COMPATIBLE
*
*     AUTHOR         H.R.RENSHALL, DD, CERN.            09/05/78.
*
*     MODIFIED       12/09/81 BY ADDITION OF MODELS 3081 AND 7880.
*     MODIFIED       11/09/82 CORRECT 7880 FOR INTERNAL LABEL 3033!
*     MODIFIED       04/01/86 BY ADDITION OF 3090 AND 7880 TO 7890
*     MODIFIED       12/02/92 BY ADDITION OF 9000 (ALL MODELS HAVE
*                    THE SAME CVTID OF 9021. AT CERN RETURN 9900).
*
*            FORTRAN CALLABLE FUNCTION TO IDENTIFY CPU MODEL.
*
*      INTEGER*4 ISIT
*      REAL*4    VAL
*      CALL WHICH ( ISIT, VAL)
*
*          WILL RETURN   ISIT ( INTEGER)  VAL ( CHARACTER) FOR MODEL
*
*                           0           '    '                  UNKNOWN
*
*                           4           '/168'             168
*
*                           5           '3032'            3032
*
*                           6           '3081'            3081
*
*                           7           '7890'            7890-MVS
*
*                           8           '3090'            3090-MVS
*
*                          10           '7890'            7890-VM CMS
*
*                          11           '3090'            3090-VM CMS
*
*                          14           '9000'            9000-MVS
*
*                          15           '9000'            9000-VM CMS
*
*                          16           '9900'            9000-CERN VM
*
*          BY EXAMINING THE CVTMDL FIELD, PREFIX -6 IN THE CVT.
*
*          NEW MODELS AND RETURN VALUES MAY BE ADDED AS LINES IN THE
*          MODELS TABLE WITH NO OTHER MODS TO THE CODE.
*
*          NOTE USES THE EXPANDED MACROS #ROUTINE, #EQUATE AND
*          #RETURN FROM THE RUTHERFORD MACRO LIBRARY.
*
*WHICH    #ROUTINE BASE=(R13),NAME=WHICH     MACRO EXPANDED HERE
          ENTRY     WHICH
*                                       DECLARE THE EXTERNAL NAME
          DS        0H                  ROUTINE ENTRY POINT
          USING     WHICH,15
*                                       INITIAL ADDRESSABILITY
          B         #RTA0001
*                                       BRANCH PAST NAME
          DC        AL1(#RTA0001-*-1)
*                                       LENGTH OF NAME
          DC        C'WHICH'            NAME
#RTA0001  DS        0H                  ALIGN FOR INSTRUCTION
          STM       14,12,12(13)        STORE CALLER'S REGISTERS
          LR        12,13               HOLD CALLER'S SA ADDRESS
          CNOP      0,4                 ALIGN ON WORD
          BAL       13,#RTE0001
*                                       BRANCH AND SET 13
#RTD0001  DS        0F                  INVENTED NAME FOR SAVEAREA
          DROP      15                  DROP OLD BASE
          USING     #RTD0001,13
*                                       ADDRESS VIA SAVEAREA
          DS        (72)X                   SAVEAREA
#RTE0001  DS        0H                  END OF SAVEAREA
          ST        12,4(0,13)          LINK OLD SA DOWN
          ST        13,8(0,12)          LINK NEW SA UP
*        #EQUATE                                   EXPANDED MACRO
*        SYMBOLIC NAMES FOR GENERAL REGISTERS
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         LTR    R2,R1              TEST IF NO PARAMETER LIST
         BZ     RETURN             DO NOTHING AND RETURN
         LA     R0,0               SET UP DEFAULT VALUE ISIT=0
         L      R2,0(R1)           GET FIRST PARAMETER ADDRESS
         ST     R0,0(R2)           LOAD DEFAULT VALUE OF ISIT
         L      R6,16              GET CVT ORIGIN INTO R6
         S      R6,CVTMDL          SUBTRACT OFFSET TO POINT TO CVTMDL
         LA     R3,MODELS-LMODEL   POINT TO BEFORE MODEL TABLE
         LA     R4,LMODEL          SET UP INCREMENT
         LA     R5,MODELEND        POINT PAST LAST MODEL
         USING  MODEL,R3           ADDRESS MODEL TABLE
LOOP     DS     0H                 LOOP OVER ALL MODELS IN TABLE
         BXH    R3,R4,UNKNOWN      EXIT IF MODEL NOT IN TABLE
         CLC    0(2,R6),MODELID    COMPARE CVTMDL WITH TABLE ENTRY
         BNE    LOOP
         MVC    0(4,R2),ISIT       LOAD ISIT ARGUMENT
         L      R0,VAL             STORE VAL IN R0
         B      FINAL              GO FOR LAST ARGUMENT
UNKNOWN  L      R0,BLANK           HERE WHEN MODEL NOT IN TABLE
FINAL    LTR    R2,R2              WAS SECOND ARGUMENT GIVEN
         BM     RETURN             RETURN IF NOT
         L      R2,4(R1)           GET 2ND ARGUMENT ADDRESS
         ST     R0,0(R2)           LOAD SECOND ARGUMENT
*RETURN   #RETURN RC=0              AND RETURN     EXPANDED MACRO
RETURN    XR        15,15               CLEAR RETURN CODE
          L         13,4(13)            PICK UP OLD SA ADDRESS
          L         14,12(13)           RESTORE REGISTER 14
          LM        2,12,28(13)         RELOAD CALLER'S REGISTERS
**        MVI       12(13),X'FF'
*                                       SET TRACEBACK FLAG
          BR        14                  RETURN TO CALLER
CVTMDL   DC     F'6'               OFFSET OF CVTMDL FROM CVT ORIGIN
BLANK    DC     X'40404040'        BLANK FOR MODEL NOT IN TABLE
MODELS   EQU    *                  NOW FOLLOWS TABLE VALUES
         DC     X'016800000000000461F1F6F8'    ISIT=4,VAL='/168'
         DC     X'3032000000000005F3F0F3F2'    ISIT=5,VAL='3032'
         DC     X'3081000000000006F3F0F8F1'    ISIT=6,VAL='3081'
#if defined(CERNLIB_IBMMVS)
         DC     X'3033000000000007F7F8F9F0'    ISIT=7,VAL='7890'!!!
         DC     X'3090000000000008F3F0F9F0'    ISIT=8,VAL='3090'
         DC     X'902100000000000EF9F0F0F0'    ISIT=14,VAL='9000'
#endif
#if !defined(CERNLIB_IBMMVS)
         DC     X'303300000000000AF7F8F9F0'    ISIT=10,VAL='7890'!!!
         DC     X'309000000000000BF3F0F9F0'    ISIT=11,VAL='3090'
#endif
#if (!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_CERN))
         DC     X'902100000000000FF9F0F0F0'    ISIT=15,VAL='9000'
#endif
#if (!defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_CERN))
         DC     X'9021000000000010F9F0F0F0'    ISIT=16,VAL='9900'
#endif
*
*        INSERT ANY NEW MODELS HERE
*
MODELEND EQU *                     END OF TABLE ENTRIES
MODEL    DSECT                     DSECT FOR MAPPING INTO TABLE
MODELID  DS     XL4                TO COMPARE WITH CVTMDL
ISIT     DS     XL4                4 BYTE ISIT VALUE TO RETURN
VAL      DS     XL4                4 CHARACTER VAL TO RETURN
LMODEL   EQU    *-MODEL            LENGTH OF TABLE ENTRY
         END
#endif
