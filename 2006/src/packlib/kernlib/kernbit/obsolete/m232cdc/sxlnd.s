*
* $Id: sxlnd.s,v 1.1.1.1 1996/02/15 17:47:39 mclareni Exp $
*
* $Log: sxlnd.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:39  mclareni
* Kernlib
*
*
          IDENT       SXLND
          ENTRY       SXLND
************************************************************************
*                                                                      *
* SUBROUTINE SXLND (WORDS,NWORDS)                                      *
*                                                                      *
* CONVERT THE FIRST NWORDS ELEMENTS OF WORDS FROM CDC SHORT   (60-BIT) *
* FLOATING POINT TO NORD NORMALISED LONG FLOATING POINT (48-BIT) AND   *
* STORE THE RESULTS BACK IN WORDS. THE NORD 48-BIT NUMBERS ARE PLACED  *
* RIGHT JUSTIFIED WITH ZERO FILL IN THE ELEMENTS OF WORDS. IF NWORDS   *
* IS LESS THAN ONE A DO NOTHING RETURN IS MADE.                        *
* NORD WORD IS 32 BIT MANTISSA,15 BIT EXPONENT (BIAS OF 2**14),SIGN BIT*
* CDC  WORD IS 48 BIT MANTISSA,11 BIT EXPONENT (BIAS OF 2**10),SIGN BIT*
* NORD NUMBER RANGE IS 10**-4920 TO 10**4920                           *
* CDC  NUMBER RANGE IS 10**-293  TO 10**322                            *
* CDC + OR -VE OVERFLOWS ARE SET TO +/-10**4920 IN NORD FORMAT         *
* CDC + OR -VE UNDERFLOWS TRANSLATE TO +/-10**-293 IN NORD FORMAT
* CDC INDEFINITE NUMBERS ARE TRANSLATED TO MAX/MIN NORD NUMBERS        *
* THE NORD OUTPUT NUMBERS ARE NORMALISED IF THE CDC INPUT NUMBERS WERE *
*                                                                      *
*                                        H.RENSHALL/DD/CERN 80/09/17   *
*                                                                      *
************************************************************************
 TRACE    VFD         30/5HSXLND,30/3
 SXLND    DATA        0                .STORAGE FOR RETURN JUMP ADDRESS
          SB1         X1               .B1=ADDRESS OF WORDS(1)
          SA1         A1+1             .X1=ADDRESS OF N
          SA1         X1               .X1=NUMBER OF WORDS TO TRANSLATE
          SB2         X1+B1            .B2=ADDRESS OF WORDS(NWORDS+1)
          LE          B2,B1,SXLND      .DO NOTHING RETURN IF N LE ZERO
          MX0         1                .SET BIT 60
          LX0         48               .SHIFT TO BIT 48
          MX5         1                .SET LEFT 1-BIT
          LX5         1                .RIGHT ADJUST
          SB3         16432            .NORD BIAS (2**14) + 48 FOR DEC P
 FLPT     SA2         B1               .SET X2 = NEXT CDC NUMBER
          BX4         X2               .COPY NUMBER TO X4
          AX4         59               .FILL X4 WITH SIGN BIT
          BX7         X4-X2            .COMPLEMENT TO X7 IF NEGATIVE
          ZR          X7,RESTORE       .ALLOWS FOR CDC +-0.
          OR          X2,OUTRANGE      .TEST IF CDC NUMBER IN RANGE (760
          UX7         B5,X7            .UNPACK +VE MANT TO X7,EXP TO B5
          AX7         16               .REDUCE MANTISSA TO 32 BITS
          SX6         B5+B3            .ADD NORD BIAS (2**14) +48 FOR DE
          LX6         32               .POSITION AS NORD EXPONENT
          BX6         X6+X7            .OR MANT AND EXP INTO X6
          BX4         X0*X4            .GET SIGN BIT IN BIT 48 OF X4
          BX7         X6+X4            .OR IN THE SIGN BIT INTO X7
          ZR          RESTORE          .JUMP OVER OUT OF RANGE
 OUTRANGE SA2         LIMIT            .SET LARGEST NORD NUMBER
          BX4         X0*X4            .GET SIGN BIT IN BIT 48 OF X4
          BX7         X2+X4             OR SIGN INTO LARGEST NORD NUMBER
 RESTORE  SA7         B1               .STORE CONVERTED NUMBER (-0. TO +
          SB1         B1+X5            .ADD 1 TO FIRST ADDRESS
          LT          B1,B2,FLPT       .INSTACK LOOP TILL ALL CONVERTED
          EQ          SXLND            .RETURN
 LIMIT    DATA        B00003777777777777776 .LARGEST NORD FP NUMBER
          END
