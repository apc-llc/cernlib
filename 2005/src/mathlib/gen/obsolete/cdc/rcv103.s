*
* $Id: rcv103.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: rcv103.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_CDC)
          IDENT  RCV103
          ENTRY  RCV103
*     SUBROUTINE RCV103
          ENTRY  IXZ103
*     FUNCTION IXZ103(LUN)
          ENTRY  UXZ103
*    ,SUBROUTINE UXZ103(LOCUSR)
          SST
*
 MSG1     DATA   H*RCV103 - RM ERROR =*
 RMNR     DATA   H*      ON *
 LFN      DATA   0
 MSG2     DATA   L*          PRECEDING ERROR UNCLEARED*
 FIES     DATA   0
 PARAM    VFD    60/PLIST
 PEND     DATA   0
 PLIST    BSS    0
 LASTES   DATA   0
 INTLFN   DATA   0
 FITADR   DATA   0
 OUTLFN   DATA   0
 ENTRY1   DATA   H* RCV103 -*
 ENTRY2   DATA   H* IXZ103 -*
 TRACE    VFD    60/TRACE3
 UFLAG    DATA   0
*
*         TO BE ENTERED WHEN A FTN ERROR 103 IS ENCOUNTERED
 TRACE1   VFD    42/0HRCV103,18/RCV103
RCV103    EQ     *+1BS17
          SX6    A1          X6=FIT ADDRESS
          BX1    X6
          SA6    FITADR
          FETCH  X1,IRS,X6
          SA6    FIES        SAVE CURRENT RM ERROR
*
*         SEND MSG1 TO DAYFILE
*         TRANSFORM INTEGER IN DISPLAY CODE
          BX3    X6
          MX6    0
          MX0    60-3
          SB5    3           ONLY 3 DIGITS IN RM ERROR
          LX3    51
          SB1    1
          SB4    1R0
 COD1     BSS    0
          LX3    3
          BX4    -X0*X3
          SX5    X4+B4
          LX6    6
          IX6    X6+X5
          SB5    B5-B1
          NZ     B5,COD1
          LX6    42
          MX0    18
          SA5    RMNR
          BX4    -X0*X5
          BX6    X0*X6
          IX6    X4+X6
          SA6    A5          STORE RMNR
          SA2    X1
          BX6    X2
          SA6    LFN         SAVE LFN NAME
          MESSAGE MSG1
*
          SA5    LASTES
          SA2    ENTRY1
          NZ     X5,UNCLEAR  JUMP IF PREVIOUS ERROR UNCLEARED
          SA4    FIES
          BX6    X4
          SA6    A5          SAVE FIES
          SA5    UFLAG
          ZR     X5,RETURN   JUMP, NO USER SUBROUTINE
*
*         A USER SUBROUTINE HAS TO BE CALLED.
*         FIND THE INTERNAL NAME OF THE FILE IN ERROR.
          SA1    FITADR
          MX0    42
          SB7    1
          SA2    B7+B7
 LOOP     BSS    0
          ZR     X2,NOMORE
          BX3    -X0*X2      GET FIT ADDRESS
          IX4    X3-X1
          ZR     X4,FOUND    JUMP, INTERNAL LFN FOUND
          SA2    A2+B7
          EQ     LOOP
*
*         PREPARE ARGUMENTS TO BE PASSED TO THE USER SUBROUTINE.
 NOMORE   BSS    0
 FOUND    BSS    0
          BX6    X0*X2
          SA6    INTLFN
          SA3    LFN
          BX7    X3
          SA7    OUTLFN
          SA1    PARAM
 RJUSER   DATA   0           CALLS THE USER SUBROUTINE
 RETURN   BSS    0
          EQ     RCV103
*
*         PRECEEDING ERROR UNCLEARED, SEND MSG2 TO DAYFILE AND ABORT
 UNCLEAR  BSS    0
          BX6    X2
          SA6    MSG2        STORE CURRENT ENTRY IN MSG2
          MESSAGE  MSG2
          MX6    0
          RJ     =XSYSEND.
          ABORT
*
*         J=IXZ103 (LUN)  CHECKS IF AN ERROR HAS OCCURED ON LUN.
*         J RETURNS THE RM ERROR WHICH IS RESPONSIBLE FOR FTN ERROR 103.
 TRACE2   VFD    42/0HIXZ103,18/IXZ103
 IXZ103   EQ     *+1BS17
          SA5    LASTES
          MX6    0
          ZR     X5,IXZ103   NO ERROR, RETURN
          MX6     1
          BX1     X1+X6
 +        RJ     =XGETFIT.   GET FIT ADDRESS IN X1
 -        VFD    30/TRACE2
          SA4    FITADR
          SA2    ENTRY2
          IX3    X4-X1       COMPARE FITADR WITH CURRENT FIT ADDRESS
          NZ     X3,UNCLEAR  JUMP, UNCLEARED ERROR FOR ANOTHER FILE
          STORE  X1,ES=0     CLEAR FIES
          SA5    LASTES
          BX6    X5
          MX7    0
          SA7    A5          CLEAR LASTES
          EQ     IXZ103
*
*         PREPARE LINK TO USER RECOVERY SUBROUTINE
 TRACE3   VFD    42/0HUXZ103,18/UXZ103
 UXZ103   EQ     *+1BS17
          SA2    X1          X2=RECOVERY SUBROUTINE ADDRESS.
          MX6    0
          ZR     X2,NORJ     X2=0, RJUSER WILL NOT BE USED
          LX2    30
          MX6    1
          SA3    TRACE
          LX6    55          CODE FOR RJ INSTRUCTION
          BX2    X6+X2
          BX6    X2+X3
          SA6    RJUSER
 NORJ     BSS    0
          SA6    UFLAG
          EQ     UXZ103
          END
#endif
