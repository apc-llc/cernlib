*
* $Id: sndsx.s,v 1.1.1.1 1996/02/15 17:47:39 mclareni Exp $
*
* $Log: sndsx.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:39  mclareni
* Kernlib
*
*
          IDENT       SNDSX
          ENTRY       SNDSX
************************************************************************
*                                                                      *
* SUBROUTINE SNDSX (WORDS,NWORDS)                                      *
*                                                                      *
* CONVERT THE FIRST NWORDS ELEMENTS OF WORDS FROM NORD SHORT (32-BIT)  *
* FLOATING POINT TO CDC NORMALISED SHORT FLOATING POINT (60-BIT) AND   *
* STORE THE RESULTS BACK IN WORDS. THE NORD 32-BIT NUMBERS ARE ASSUMED *
* RIGHT JUSTIFIED WITH ZERO FILL IN THE ELEMENTS OF WORDS. IF NWORDS   *
* IS LESS THAN ONE A DO NOTHING RETURN IS MADE.                        *
* NORD WORD IS 22 BIT MANTISSA,9 BIT EXPONENT (BIAS OF 2**8),SIGN BIT  *
* NORD WORD HAS IMPLICIT BIT 23 IN MANTISSA FOR NON-ZERO NUMBERS       *
* CDC  WORD IS 48 BIT MANTISSA,11 BIT EXPONENT (BIAS OF 2**10),SIGN BIT*
* NORD NUMBER RANGE IS 10**-76 TO 10**76                               *
* CDC  NUMBER RANGE IS 10**-293  TO 10**322                            *
* ALL NORD NUMBERS CONVERT EXACTLY                                     *
*                                                                      *
*                                        H.RENSHALL/DD/CERN 80/09/17   *
*                                                                      *
************************************************************************
 TRACE    VFD         30/5HSNDSX,30/3
 SNDSX    DATA        0                .STORAGE FOR RETURN JUMP ADDRESS
          SB1         X1               .B1=ADDRESS OF WORDS(1)
          SA1         A1+1             .X1=ADDRESS OF N
          SA1         X1               .X1=NUMBER OF WORDS TO TRANSLATE
          SB2         X1+B1            .B2=ADDRESS OF WORDS(NWORDS+1)
          LE          B2,B1,SNDSX      .ERROR RETURN IF N LE ZERO
          MX0         38               .FORM LEFT 38 BITS MASK
          MX1         51               .SET MASK
          MX7         1                .SET BIT 60
          LX7         23               .SHIFT TO BIT 23
 FLPT     SA2         B1               .SET X2 = NEXT NORD NUMBER
          ZR          X2,ENDLOOP       .LEAVE UNCHANGED IF ZERO
          BX4         X2               .COPY NORD NUMBER INTO X4
          BX2         -X0*X2           .MASK MANTISSA
          BX2         X2+X7            .OR MISSING NORD BIT IN MANTISSA
          LX4         28               .LEFT ADJUST NORD NUMBER
          AX4         50               .RIGHT ADJ-DROP MANT-EXTEND SIGN
          BX3         -X1*X4           .MASK 9-BIT EXPONENT INTO X3
          AX4         9                .MAKE X4 A COPY OF THE SIGN BIT
          SB5         X3-279           .SUB (2**8 BIAS + 23 TO CONV FRAC
          PX6         X2,B5            .PACK AS A FLPT NUMBER
          NX6         X6,B5            .NORMALISE THE NUMBER
          BX6         X4-X6            .COMPLEMENT IF NEGATIVE
          SA6         B1               .SAVE CONVERTED NUMBER
 ENDLOOP  SB1         B1+1             .ADD 1 TO FIRST ADDRESS
          LT          B1,B2,FLPT       .INSTACK LOOP TILL ALL CONVERTED
          EQ          SNDSX            .RETURN
          END
