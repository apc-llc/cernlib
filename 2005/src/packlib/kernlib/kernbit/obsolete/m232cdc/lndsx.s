*
* $Id: lndsx.s,v 1.1.1.1 1996/02/15 17:47:39 mclareni Exp $
*
* $Log: lndsx.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:39  mclareni
* Kernlib
*
*
          IDENT       LNDSX
          ENTRY       LNDSX
************************************************************************
*                                                                      *
* SUBROUTINE LNDSX (WORDS,NWORDS)                                      *
*                                                                      *
* CONVERT THE FIRST NWORDS ELEMENTS OF WORDS FROM NORD LONG (48-BIT)   *
* FLOATING POINT TO CDC NORMALISED SHORT FLOATING POINT (60-BIT) AND   *
* STORE THE RESULTS BACK IN WORDS. THE NORD 48-BIT NUMBERS ARE ASSUMED *
* RIGHT JUSTIFIED WITH ZERO FILL IN THE ELEMENTS OF WORDS. IF NWORDS   *
* IS LESS THAN ONE A DO NOTHING RETURN IS MADE.                        *
* NORD WORD IS 32 BIT MANTISSA,15 BIT EXPONENT (BIAS OF 2**14),SIGN BIT*
* CDC  WORD IS 48 BIT MANTISSA,11 BIT EXPONENT (BIAS OF 2**10),SIGN BIT*
* NORD NUMBER RANGE IS 10**-4920 TO 10**4920                           *
* CDC  NUMBER RANGE IS 10**-293  TO 10**322                            *
* NORD NUMBERS GT 10**322 ARE SET TO 10**322 IN CDC FORM               *
* NORD NUMBERS LT 10**-293 ARE SET TO 10**-293 IN CDC FORM             *
*                                                                      *
*                                        H.RENSHALL/DD/CERN 80/09/17   *
*                                                                      *
************************************************************************
 TRACE    VFD         30/5HLNDSX,30/3
 LNDSX    DATA        0                .STORAGE FOR RETURN JUMP ADDRESS
          SB1         X1               .B1=ADDRESS OF WORDS(1)
          SA1         A1+1             .X1=ADDRESS OF N
          SA1         X1               .X1=NUMBER OF WORDS TO TRANSLATE
          SB2         X1+B1            .B2=ADDRESS OF WORDS(NWORDS+1)
          LE          B2,B1,LNDSX      .DO NOTHING RETURN IF N LE ZERO
          MX0         28               .FORM LEFT 28 BITS MASK
          MX1         45               .FORM LEFT 45 BITS MASK
          MX5         9                .FORM LEFT 9 BITS MASK-CDC EXP
          LX5         10               .SHIFT 9 BIT MASK TO BITS 2-10
          MX7         1                .FORM LEFT 1 BIT MASK
          LX7         48               .SHIFT TO BIT 48-CDC MANTISSA
 FLPT     SA2         B1               .SET X2 = NEXT NORD NUMBER
          ZR          X2,ENDLOOP       .LEAVE UNCHANGED IF ZERO
          BX4         X2               .COPY NORD NUMBER TO X4
          BX2         -X0*X2           .MASK OFF MANTISSA
          LX4         12               .LEFT ADJUST NORD NUMBER
          AX4         44               .RIGHT ADJUST WITH SIGN EXTEND
          BX3         -X1*X4           .MASK INTO X3 THE 15 BIT EXPONENT
          AX4         15               .MAKE X4 A COPY OF THE SIGN BIT
          SX3         X3-16416         .SUBTR (2**14 BIAS + 32 TO CONV F
          SB5         X3               .COPY 18-BIT SIGNED EXPONENT TO B
          AX3         10               .RIGHT SHIFT-DROP 10 BITS,EXTEND
          ZR          X3,NOOVFL        .TEST ON MIXED BITS IE CDC EXP OF
          AX3         7                .MAKE X3 A COPY OF THE EXPONENT S
          BX3         X3-X5            .SET LARGEST OR SMALLEST CDC EXPO
          SB5         X3               .COPY THIS EXPONENT INTO B5
          BX2         X7               .SET BIT 48 AS THE CDC MANTISSA
 NOOVFL   PX6         X2,B5            .PACK AS A FLOATING POINT NUMBER
          NX6         X6,B5            .NORMALISE THE NUMBER
          BX6         X4-X6            .COMPLEMENT IF NEGATIVE
          SA6         B1               .SAVE CONVERTED NUMBER
 ENDLOOP  SB1         B1+1             .ADD 1 TO FIRST ADDRESS
          LT          B1,B2,FLPT       .INSTACK LOOP TILL ALL CONVERTED
          EQ          LNDSX            .RETURN
          END
