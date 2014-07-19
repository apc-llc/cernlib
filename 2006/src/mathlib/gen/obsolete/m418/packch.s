*
* $Id: packch.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: packch.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT PACKCH
#include "entftn.inc"
 PACKWD   DATA        37777600B         .ADDRESS/SHIFT FOR PACKING
 PACKN    DATA        0                 .CURRENT INPUT CHARACTER ADDRESS
 UPWORD   DATA        37777600B         .ADDRESS/SHIFT FOR UNPACKING
 UPN      DATA        0                 .CURRENT OUPPUT CHARACTER ADDRES
,S
 PACKMX   DATA        377777B           .MAX CHAR ADDRESS I/P 131 071
 UPMX     DATA        377777B           .MAX CHAR ADDRESS O/P 131071
          SPACE       2
*         MACRO TO DIVIDE X1 BY TEN AND CALCULATE 6*(6-(N-10*K)) = L
*         N IS IN X1
*         K IS IN X5 = N/10
*         L IS IN X4 AND B4
          SPACE       2
 TENTH    MACRO       WORDN
          SA2         =0.1              .FACTOR FOR MULTIPLACTION
          SA5         =0.01             .TERM FOR ROUNDING
          BX6         X1               .MOVE N TO CURRENT ADDRESS
          PX3         B0,X1            .FLOAT N
          SA6         WORDN            .STORE CURRENT CHARACTER ADDRESS
          FX4         X3*X2            .X4 = 0.1*N
          FX5         X5+X4            .ADD ROUNDING FACTOR
          UX3         B4,X5
          SB5         1
          SB6         3
          LX5         B4,X3            .X5 = FIX (N/10) = K
          LX2         X5,B5            .2*K
          LX3         X5,B6            .8*K
          SX4         9
          IX2         X2+X3            .10*K
          IX3         X1-X2            .N-10*K
          SB4         2
          SB5         1
          IX4         X4-X3            .9-(N-10*K) = CHAR IN WORD
          LX2         X4,B4            .2*SH
          LX3         X4,B5            .4*SHIFT
          IX4         X2+X3            .6*CHAR = L
          SB4         X4
          ENDM
          SPACE       2
*         PICK UP WORD AND OAND OUT THE OLD CHARACTER AND OR IN THE NEW.
*         TO TAKE CHARACTER IN X3 (ZZZZZZZZZX) AND PLACE IN
*         THE WORD X2 (ABCDEFGHIJ).
*         X4 = B4 = 6*(9-(11-10)) = 48
*         NOTE 0 = 6 BITS OF ZERO AND + IS 6 BITS OF ONES (63)
*         AT END OF MACRO STORE CURRENT SHIFT AND ADDRESS IN PACKWD
          SPACE       2
 PACK     MACRO
          MX0         54               .X0 = +++++++++0
          SX5         A2               .CURRENT ADDRESS IS 0000000AAA
          LX6         X0,B4            .X6 = +0++++++++
          BX3         -X0*X3           .X3 = 000000000X
          BX2         X2*X6            .X2 = A0CDEFGHIJ
          LX7         X3,B4            .X7 = 0X00000000
          LX5         6                .SHIFT 000000AAA0
          BX6         X2+X7            .X6 = AXCDEFGHIJ
          IX7         X5+X4            .ADD SHIFT 000000AAAS
          SA6         A2               .STORE RESULT
          SA7         PACKWD
          ENDM
          SPACE       2
*         MASK OUT CHARACTER IN X2
*         STROE CURRENT ADDRESS AND SHIFT IN UPWORD
          SPACE       2
 UNPACK   MACRO
          SX5         A2
          MX0         54               .MASK
          LX5         6
          IX7         X5+X4
          AX3         X2,B4            .SHIFT CHARACTER TO BITS 0-5
          SA7         UPWORD
          BX6         -X0*X3           .AND OUT BITS /-5
          ENDM
          SPACE       2
*         TO LOOK AT WORD AND SET X2 TO ADDRESS FOR NEXT CHARACTER
*         AND B4 (X4) TO CURRENT SHIFT
          SPACE       2
 TEST     MACRO       WORD,SAME,WORDN,MAX,OK,OV,START
          SA3         WORDN            .ADD ONE TO CURRENT CHARACTER ADD
          SA2         MAX              .X2 = MAXCH COUNT FOR ARRAY
          SA1         WORD             .GET LAST CONDITIONX
          MX0         54
          IX2         X3-X2            .TEST FOR MAC CHA COUNT
          NG          X2,OK            .JUMP IF NOT EXCEEDED
          SA1       B7
 OV       RJ          =XEXIT           .ELSE GO TO EXIT OR USER ROUTINE
 -        LT          B0,B1,START-1
          EQ          B0,B0,START      .RETURN  ASSUME USER S/R HAS FUN
 OK       SX6         X3+1
          BX2         -X0*X1           .ISLOLATE SHIFT
          AX1         6                .SET X1 TO LAST ADDRESS
          SA6         A3
          SX4         X2-6             .REDUCE SHIFT BY ONE CHAR
          PL          X4,SAME          .JUMP IF SAME WORD
          SX4         54               .ELSE RESET X4 TO 54
          SX1         X1+1             .AND ADD ONE TO ADDRESS
 SAME     SA2         X1               .BRING CURRENT WORD TO X2
          SB4         X4               .PUT SHIFT IN B4
          ENDM
          SPACE       2
*         TO INSERT BUFFER SIZE INTO TABLE AND ENTRY POINT OF
*         USERS OVERFLOW ROUTINE INPLACE OF RJ   EXIT
          SPACE       2
 SIZE     MACRO       MAX,OV
          SA1         B1               .MAXIMUM BUFFER SIZE
          SA4         OV               .0100AAAAAA0700BBBBBB (A = EXIT)
          MX0         43               . MASK FOR SIZE OF BUFFER
          SX3         B2               .ADDRESS OF USER S/R 0..0UUUUUU
          BX6         -X0*X1           .REDUCE MAX SIZE TO 2**17-1
          MX0         18               .++++++000000000000
          SA6         MAX              .STORE MAXIMUM CHARACTER IN MAX
          LX0         48               .0000++++++0000000000
          LX3         30               .ADDRESS   0000UUUUUU0000000000
          BX6         -X0*X4           .01000000000700BBBBBB
          BX7         X6+X3            .0100UUUUUU0700BBBBBB
          SA7         A4               .STORE NEW RJ INSTRUCTION
          ENDM
          EJECT
          SPACE       2
*         ENTRY TO PACK CHARACTER INTO CHARTER POSITION N OF ARRAY A.
          SPACE       2
          ENTRY       PACKCH
* SUBROUTINE PACKCH(I,ICH,N)
 +        VFD         42/0HPACKCH,18/3
          ENTFTN PACKCH,3
          SA1         B3
          TENTH       PACKN
          SA2         B1+X5            .X2 = A(2) = ABCDEFGHIJ
          SA3         B2               .X3 = Q = ZZZZZZZZZX
          PACK
          EQ          B0,B0,PACKCH     .RETURN
          EJECT
          SPACE       2
*         PICK UP WORD AND OBTAIN CHARACTER.  STORE IN X6
          SPACE       2
          ENTRY       ICHAR
* FUNCTION ICHAR(Y,N)
 +        VFD         42/0HICHAR,18/2
          ENTFTN ICHAR,2
          SA1         B2
          TENTH       UPN
          SA2         B1+X5            .PICK UP WORD IN ARRAY
          UNPACK
          EQ          B0,B0,ICHAR      RETURN
          EJECT
          SPACE       2
*         ENTRY TO PACK NEXT CHARACTER 2NTO ARRAY.  LAST ADDRESS IS USED
          SPACE       2
          ENTRY       PKNXCH
* SUBROUTINE PKNXCH(ICH)
 +        VFD         42/0HPKNXCH,18/01
          ENTFTN PKNXCH,1
          TEST        PACKWD,SAMEA,PACKN,PACKMX,OKA,PACKOV,PKNXCH
          SA3         B1               .PICK UP WORD TO BE PACKED
          PACK
          EQ          B0,B0,PKNXCH     .RETURN
          EJECT
          SPACE       2
*         ENTRY TO OBTIAN NEXT CHARACTER FROM INPUT ARRAY.
          SPACE       2
          ENTRY       INXCH
* FUNCTION INXCH(DUMMY)
 +        VFD         42/0HINXCH,18/01
          ENTFTN INXCH,0
          TEST        UPWORD,SAMEB,UPN,UPMX,OKB,UPOV,INXCH
          UNPACK
          EQ          B0,B0,INXCH      .RETURN
          EJECT
          SPACE       2
*         ENTRY TO PROVIDE CURRENT CHARACTER ADDRESS IN THE BUFFER
*         BEING UNPACKED.
          SPACE       2
          ENTRY       IINN
* FUNCTION IINN(DUMMY)
 +        VFD         42/0HIINN,18/01
          ENTFTN IINN,0
          SA1         UPN
          BX6         X1
          EQ          B0,B0,IINN       .RETURN
          SPACE       5
*         ENTRY TO PROVIDE CURRENT CHARACTER ADDRESS IN THE BUFFER
*         BEING PACKED
          SPACE       2
          ENTRY       IOUTN
* FUNCTION IOUTN(DUMMY)
 +        VFD         42/0HIOUTN,18/01
          ENTFTN IOUTN,0
          SA1         PACKN
          BX6         X1
          EQ          B0,B0,IOUTN
          EJECT
          SPACE       2
*         ENTRY TO SET SIZE OC OUTPUT BUFFER AND EXTERNAL TO BE
*         CALLED WHEN THE BYFFER IS FULL
          SPACE       2
          ENTRY       OUTSZE
* SUBROUTINE OUTSZE(NEWMAX,USER)
 +        VFD         42/0HOUTSZE,18/02
          ENTFTN OUTSZE,2
          SIZE        PACKMX,PACKOV
          EQ          B0,B0,OUTSZE
          SPACE       2
*         ENTRY TO SET SIZE OF INPUT BUFFER AND EXTERNAL TO BE
*         CALLED WHEN THE BUFFER IS EXHAUSTED
          SPACE       2
          ENTRY       INSZE
* SUBROUTINE INSZE(NEW,USERI)
 +        VFD         42/0HINSZE,18/02
          ENTFTN INSZE,2
          SIZE        UPMX,UPOV
          EQ          B0,B0,INSZE
          SPACE       2
          END
