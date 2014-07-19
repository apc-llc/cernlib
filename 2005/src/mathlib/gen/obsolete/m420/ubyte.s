*
* $Id: ubyte.s,v 1.1.1.1 1996/04/01 15:03:21 mclareni Exp $
*
* $Log: ubyte.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:21  mclareni
* Mathlib gen
*
*
          IDENT       UBYTE            (19/4/71)
*         *************************************
#include "entftn.inc"
*         *************************************
          SPACE 2
* THIS ROUTINE UNPACKS BINARY INFORMATION CONTAINED IN AN INPUT RECORD
* BY EXPANDING THE RECORD SO THAT ONLY BTSPW (THIRD INPUT PARAMETER)
* BITS PERWORD, RIGHT ADJUSTED AND ZERO FILL, ARE PLACED IN THE OUTPUT
* RECORD. THE OUTPUT RECORD IS PLACED IN THE INPUT RECORD BUFFER AND IT
* IT IS ASSUMED THAT THE INPUT RECORD RECORD BUFFER IS LARGE ENOUGH TO
* CONTAIN THE OUTPUT RECORD. IT IS ALSO ASSUMED THAT ANY BITS AT THE
* END OF THE INPUT RECORD WHICH DO NOT COMPRISE A WHOLE CM WORD ARE
* LEFT ADJUSTED IN THE LAST WORD OF THE RECORD, THE REMAINING FILL BITS
* ARE NOT EXAMINED. ON OUTPUT ANY REMAINING BITS LESS THAN BTSPW ARE
* EXPANDED TO BTSPW BY ZERO FILL AT THE RIGHT AND PLACED IN THE LAST
* WORD OF THE OUTPUT RECORD.
* FORTRAN CALLING SEQUENCE IS EXPECTED BY THIS ROUTINE. THIS ROUTINE
* MAY BE CALLED BY A REGULAR FORTRAN CALL STATEMENT.-
*         CALL  UBYTE  (NBYTES,REC,BTSPW)
* THE EXPANSION TAKES PLACE BEGINNING AT THE LAST WORD OF THE INPUT
* RECORD, WORKING BACKWARDS AND TERMINATING WITH THE FIRST WORD OF THE
* INPUT RECORD.
*
* PARAMETERS SET UP BY THE CALLING ROUTINE (IN REGS B1-B3)
*         ADNBYTS     ADDRESS OF NBYTS, THE LENGTH OF THE INPUT RECORD
*                     IN 8-,16 OR 32-BIT BYTES. THIS VALUE MUST LIE
*                     BETWEEN 1 AND 3840 INCLUSIVE.
*         ADREC       ADDRESS OF THE INPUT RECORD REC.
*         ADBTSPW     ADDRESS OF BTSPW, THE NUMBER OF BITS PER WORD IN
*                     THE OUTPUT RECORD. THE VALUE OF THIS PARAMETER
*                     MUST BE-
*                     0           NO EXPANSION IS PERFORMED
*                     8,16,OR 32  THE NUMBER OF BITS PLACED IN EACH
*                                 WORD OF THE OUTPUT RECORD AS SHOWN
*                                 ABOVE
*
* REGISTERS USED BY THIS ROUTINE
*         ALL
          SPACE 5
          ENTRY    UBYTE
* SUBROUTINE UBYTE(N,A,IBITS)
 +        VFD  30/0HUBYTE,30/3
          ENTFTN UBYTE,3,X1
          SPACE 2
* CHECK THAT THE NUMBER OF UNPUT PARAMETERS IS EXACTLY THREE
          SA3  B1            .NBYTS
          BX6  X3            .NBYTS
          SX2  X1-3          .CHECK    NUM OF PARMS
          NZ   X2,UBYTE      .ERROR IF NOT THREE
          SPACE 2
*  CHECK THAT NBYTS IS .GT. 0
          BX4  X6            .NBYTS
          ZR   X3,UBYTE      .ERROR IF ZERO
          LX4  3             .MULTIPLY BYTES BY 8 FOR TOTAL BITS
          NG   X6,UBYTE      .ERROR IF NEGATIVE
          SA2  FSIXTY        .FLT PT VALUE OF SIXTY
          SA3         B3               .X3=IBITS
          SB4         X3-8
          EQ          B0,B4,UB
          LX4         1
          SB4         X3-16
          EQ          B0,B4,UB
          LX4       1
          SPACE 2
* CALCULATE TOTAL NUMBER OF BITS IN THE INPUT RECORD. ALSO THE NUMBER
* OF WHOLE WORDS AND REMAINING BITS
 UB       PX7  B0,X4
          NX7  B4,X7         .CONVERT NUM OF BITS TO FLT PT
          FX5  X7/X2         .DIVIDE BY SIXTY FOR NUMBER OF WHOLE WORDS
          UX5  B4,X5         .CONVERT TO INTEGER
          LX5  B4,X5         .X5 HAS NUM OF WHOLE WORDS IN INPUT REC
          PX6  B0,X5         .GET FLT PT VALUE OF NUM OF WHOLE WORDS
          NX6  B4,X6
          FX3  X6*X2         .MULTIPLY BY SIXTY
          FX3  X7-X3         .REMAINING BITS AFTER DIVISION BY 60
          SX2  60            .SET UP X2 FOR LOOP
          UX3  B4,X3         .CONVERT TO INTEGER
          SB7  X2            . SET UP B7 FOR LOOP
          LX7  B4,X3         .X7=0, OR LEFTOVER VALID BITS IN LAST WORD
          ZR   X7,REB4       .NO ODD BITS IN LAST WORD
          SA1  B2+X5         .SET UP X1 FOR LOOP
          SB7  X7
          LX1  B7,X1         .SET UP X1 FOR LOOP
          EQ   B0,B0,REB5
 REB4     SX5  X5-1          .NO ODD BITS IN LAST WORD
          SA1  B2+X5
* REGISTERS SET UP IN THE PRECEEDING CODE ARE-
*   X4= TOTAL BITS IN INPUT RECORD
*   X2= INTEGER VALUE 60
*   X7= NUMBER OF VALID DATA BITS IN LAST WORD OF RECORD OR 0
*   B7= NUMBER OF VALID DATA BITS IN LAST WORD OF RECORD OR 60
*   X5= NUMBER OF TOTAL WORDS IN THE INPUT RECORD -1
*   A1= ADDRESS OF LAST WORD OF THE INPUT RECORD
*   X1= CONTENTS OF LAST WORD OF THE INPUT RECORD
          SPACE 2
* CHECK THAT BTSPW IS 0, 8, 16,OR 32 AND SET UP MASKS ACCORDINGLY
 REB5     SA3  B3            .GET THIRD INPUT PARAMETER
          ZR   X3,UBYTE      .NO UNPACKING
          SX7  X3-8
          ZR   X7,REB1       .BTSPW=8
          SX7  X3-16
          ZR   X7,REB2       .BTSPW=16
          SX7  X3-32
          NZ   X7,UBYTE      .BTSPW NOT EQUAL TO 0,8,16, OR 32
          SB1  5
          SX5  37B
          EQ   B0,B0,REB3
 REB2     SB1  4             .BTSPW=16
          SX5  17B
          EQ   B0,B0,REB3
 REB1     SB1  3             .BTSPW=8
          SX5  7
* REGISTERS SET UP IN THE PRECEEDING CODE ARE-
*   X3= BTSPW, THIRD INPUT PARAMETER
*   B1= MASK FROM BTSPW
*   X5= MASK FROM BTSPW
*   X4= TOTAL BITS IN INPUT RECORD
*   X2= INTEGER VALUE 60
*   B7= NUMBER OF VALID DATA BITS IN THE LAST WORD OF THE INPUT RECORD
*   A1= ADDRESS OF LAST WORD OF THE INPUT RECORD
*   X1= CONTENTS OF LAST WORD OF THE INPUT RECORD
          SPACE 2
* GET NUMBER OF WHOLE NEW WORDS IN  OUTPUT  RECORD AND NUMBER OF
* VALID DATA BITS IN THE LAST WORD OF THE OUTPUT RECORD
 REB3     SB5  1
          BX7  X4*X5         .X7= NUM OF VALID BITS IN LAST WORD
          AX4  B1,X4         .X4= TOTAL NUM OF WORDS -1
          NZ   X7,REB6
          SX4  X4-1          .X4=NUM OF WORDS IN OUTPUT REC -1
          BX7  X3            .NUM OF DATA BITS IN THE LAST WORD OUTPUT
 REB6     IX5  X3-X7         .NUM OF BITS OF ZERO FILL IN LAST WORD
          SB3  X5            .NUMBER OF BITS ZERO FILL
          SB4  X7            .NUM OF VALID BITS IN LAST WORD OUTPUT
          SB6  B2+X4         .ADDRESS OF FIRST WORD OF OUTPUT RECORD
          SX6  B0
          SA3  X3
          SB1  A3-1          .A3 HAS VALUE 8, 16, OR 32
          MX5  1             .SET UP MASK IN X5
          AX5  B1,X5
          SB1  B1+B5
          LX5  B1,X5         .X5 HAS 8, 16, 32, ONE BITS IN RHS
          BX3  X1
          SA1  A1-B5         .GET NEXT WORD OF INPUT RECORD
          GE   B7,B4,REB21
          SPACE 2
* THERE ARE NOT ENOUGH BITS IN THE LAST INPUT WORD FOR THE FIRST OUTPUT
* WORD, SO MOVE THE BITS TO THE OUTPUT REGISTER AND FETCH THE NEXT
* INPUT WORD
 REB20    SX6  B0
          MX6  1
          SB7  B7-B5
          AX6  B7,X6
          SB7  B7+B5
          LX6  B7,X6
          BX6  X6*X3
          BX3  X1            .X3 CONTAINS CURRENT INPUT WRD
          SA1  A1-B5         .GET NEXT WORD OF INPUT RECORD
          LX6  B3,X6
          SB3  B3+B7
          SB4  B4-B7
          SB7  X2
          EQ   B0,B0,REB21
* LOOP TO CONVERT THE INPUT RECORD TO THE OUTPUT RECORD. CONVERSION
* TAKES PLACE BEGINNING AT THE LAST WORD OF THE INPUT RECORD AND
* CONTINUING UNTIL THE BEGINNING OF THE RECORD IS REACHED. THE NEW
* OUTPUT RECORD IS STORED ALTERNATIVELY FROM REGISTERS 6 AND 7 SO
* THE CODE IS HELD ESSENTIALLY IN TWO LOOPS TO ENABLE THIS TO BE
* DONE MOST EFFICIENTLY. THE REGISTERS USED IN THE LOOPS ARE LISTED
* BELOW
          SPACE 2
*         ..............................................................
*         ..............................................................
*         ..                                                          ..
*         ..              REGISTER ALLOCATION                         ..
*         ..              *******************                         ..
*         ..                                                          ..
*         ..   B5  +1                                                 ..
*         ..   B3  NUMBER OF BITS ALREADY IN X6 OR X7                 ..
*         ..   B4  NUM OF BITS YET TO BE MOVED TO THE OUTPUT REGISTER ..
*         ..   B7  NUMBER OF BITS IN INPUT REGISTER YET TO BE MOVES   ..
*         ..   A1  ADDRESS OF WORD IN INPUT REGISTER                  ..
*         ..   X1  CONTAINS NEXT    INPUT WORD                        ..
*         ..   X3  CONTAINS CURRENT INPUT WORD                        ..
*         ..   A3  8, 16, OR 32. THE THIRD INPUT PARAMETER            ..
*         ..   X6  OUTPUT REGISTER                                    ..
*         ..   X7  OUTPUT REGISTER                                    ..
*         ..   B6  ADDRESS OF CURRENT OUTPUT WORD                     ..
*         ..   X4  WORKING REGISTER                                   ..
*         ..   X0  WORKING REGISTER                                   .=
*         ..   X2  +60                                                ..
*         ..   B2  ADDRESS OF THE INPUT RECORD                        ..
*         ..   X5  HAS MASK OF 8, 16, OR 32 ONE BITS IN RHS           ..
*         ..                                                          ..
*         ..............................................................
*         ..............................................................
          SPACE 2
 REB10    SB7  X2            .X2=60
          BX3  X1            .GET NEW INPUT WORD
          SA1  A1-B5
 REB9     BX6  X5*X3         .MASK OUT BITS NEEDED FOR OUTPUT
          SB7  B7-B4         .NUM OF VALID BITS LEFT IN INPUT WORD
          SA6  B6            .SAVE OUTPUT WORD
          AX3  B4,X3         .DISCARD BITS ALREADY USED
          SB6  B6-B5         .ADDRESS OF NEXT WORD TO BE STORED
          LT   B7,B4,REB8    .ARE THERE ENOUGH BITS FOR NEXT OUTPUT WORD
 REB90    BX7  X5*X3         .MASK BITS NEEDED FOR OUTPUT
          SB7  B7-B4         .NUMBER OF VALID BITS LEFT IN INPUT WORD
          SA7  B6            .SAVE OUTPUT WORD
          AX3  B4,X3         .DISCARD BITS ALREADY USED
          SB6  B6-B5         .ADDRESS OF NEXT WORD TO BE STORED
          GE   B7,B4,REB9    .ARE THERE ENOUGH BITS FOR NEXT WORD
 REB80    LT   B6,B2,UBYTE    IS THIS THE END OF INPUT
          NZ   B7,REB110     .ARE THERE ANY BITS LEFT IN INPUT WORD
          EQ   B0,B0,REB10
 REB110   SB4  B7            .NUMBER OF BITS LEFT IN INPUT WORD
          MX0  1
          SB4  B4-B5
          AX4  B4,X0
          SB4  B4+B5
          LX0  B4,X4         .SET UP MASK IN X0
          BX6  X0*X3         .MASK REMAINING BITS INTO X6
          SB3  B4            .NUM OF BITS ALREADY IN OUTPUT WORD
          BX3  X1            .GET NEW INPUT WORD
          SB4  A3-B4         .NUMBER OF BITS STILL TO BE MOVED
          SB7  X2            .X2=60
          SA1  A1-B5         .GET NEXT INPUT WORD
 REB21    MX0  1
          SB4  B4-B5
          AX4  B4,X0
          SB4  B4+B5
          LX0  B4,X4         .PUT MASK INTO X0
          BX4  X0*X3         .GET THE REMAINING BITS INTO X4
          LX0  B3,X4         .PUT BITS INTO RIGHT POSITION OF REGISTER
          BX6  X0+X6         .ADD REMAINING BITS TO X6
          SB7  B7-B4         .NUM OF VALID BITS LEFT IN INPUT WORD
          AX3  B4,X3         .DISCARD BITS ALREADY USED
          SA6  B6            .SAVE OUTPUT WORD
          SB4  A3            .A3=8, 16, OR 32
          SB6  B6-B5         .ADDRESS OF NEXT OUTPUT WORD
          GE   B7,B4,REB90   .ARE THERE ENOUGH BITS FOR WHOLE NEW WORD
 REB8     LT   B6,B2,UBYTE    IS THIS THE END OF INPUT
          ZR   B7,REB100     .GET NEXT NEW WORD
          SB4  B7            .NUMBER OF BITS LEFT TO BE MOVED
          MX0  1
          SB4  B4-B5
          AX4  B4,X0
          SB4  B4+B5
          LX0  B4,X4         .GET MASK INTO X0
          BX7  X0*X3         .GET REMAINING BITS FROM INPTU WORD
          SB3  B4            .NUM OF BITS ALREADY IN X7
          BX3  X1            .GET NEW INPUT WORD
          SB4  A3-B4         .NUMBER OF BITS STILL TO BE MOVED
          SB7  X2            .X2=60
          SA1  A1-B5         .GET NEXT INPUT WORD
          MX0  1
          SB4  B4-B5
          AX4  B4,X0
          SB4  B4+B5
          LX0  B4,X4         .GET MASK INTO X0
          BX4  X0*X3         .GET THE REMAINING BITS INTO X4
          LX0  B3,X4         .PTU BITS INTO CORRECT POSITION IN REGISTER
          BX7  X0+X7         .ADD THESE BITS TO OTHERS
          SB7  B7-B4         .NUM OF VALID BITS LEFT IN INPUT WORD
          AX3  B4,X3         .DISCARD BITS ALREADY USED
          SA7  B6            .SAVE OUTPUT WORD
          SB4  A3            .A3= 8, 16, OR 32
          SB6  B6-B5         .ADDRESS OF NEXT OUTPUT WORD
          GE   B7,B4,REB9    .ARE THERE ENOUGH BITS FOR NEXT NEW WORD
          EQ   B0,B0,REB80
 REB100   SB7  X2            .X2=60
          BX3  X1            .GET NEW INPUT WORD
          SA1  A1-B5         .GET NEXT INPUT WORD
          EQ   B0,B0,REB90
 FSIXTY   DATA        60.0
          END
