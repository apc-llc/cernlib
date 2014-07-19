*
* $Id: sxsnd.s,v 1.1.1.1 1996/02/15 17:47:39 mclareni Exp $
*
* $Log: sxsnd.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:39  mclareni
* Kernlib
*
*
          IDENT       SXSND
          ENTRY       SXSND
************************************************************************
*                                                                      *
* SUBROUTINE SXSND (WORDS,NWORDS)                                      *
*                                                                      *
* CONVERT THE FIRST NWORDS ELEMENTS OF WORDS FROM CDC SHORT   (60-BIT) *
* FLOATING POINT TO NORD NORMALISED SHORT FLOATING POINT (32-BIT) AND  *
* STORE THE RESULTS BACK IN WORDS. THE NORD 32-BIT NUMBERS ARE PLACED  *
* RIGHT JUSTIFIED WITH ZERO FILL IN THE ELEMENTS OF WORDS. IF NWORDS   *
* IS LESS THAN ONE A DO NOTHING RETURN IS MADE.                        *
* NORD WORD IS 22 BIT MANTISSA,9 BIT EXPONENT (BIAS OF 2**8),SIGN BIT  *
* NORD WORD HAS IMPLICIT BIT 23 SET IN MANTISSA FOR NON-ZERO NUMBERS   *
* CDC  WORD IS 48 BIT MANTISSA,11 BIT EXPONENT (BIAS OF 2**10),SIGN BIT*
* NORD NUMBER RANGE IS 10**-76 TO 10**76                               *
* CDC  NUMBER RANGE IS 10**-293  TO 10**322                            *
* CDC + OR -VE NUMBERS GT 10**76  ARE SET TO +/-10**76 IN NORD FORMAT  *
* CDC + OR -VE NUMBERS LT 10**-76 ARE SET TO +/-10**-76 IN NORD FORMAT *
* CDC INDEFINITE NUMBERS ARE TRANSLATED TO MAX/MIN NORD NUMBERS        *
* THE NORD OUTPUT NUMBERS ARE NORMALISED IF THE CDC INPUT NUMBERS WERE *
*                                                                      *
*                                        H.RENSHALL/DD/CERN 80/09/17   *
*                                                                      *
************************************************************************
 TRACE    VFD         30/5HSXSND,30/3
 SXSND    DATA        0                .STORAGE FOR RETURN JUMP ADDRESS
          SB1         X1               .B1=ADDRESS OF WORDS(1)
          SA1         A1+1             .X1=ADDRESS OF N
          SA1         X1               .X1=NUMBER OF WORDS TO TRANSLATE
          SB2         X1+B1            .B2=ADDRESS OF WORDS(NWORDS+1)
          LE          B2,B1,SXSND      .DO NOTHING RETURN IF N LE ZERO
          MX5         22               .FORM LEFT 22 BITS MASK
          LX5         22               .RIGHT ADJUST 22 BIT MASK
          MX0         1                .FORM LEFT 1-BIT MASK
          LX0         32               .SET NORD SIGN BIT
          MX1         9                .FORM LEFT 9-BIT MASK
          LX1         9                .RIGHT ADJUST 9 BIT MASK
          SB3         304              .NORD BIAS (2**8) + 48 FOR DEC PN
          SB7         1                .SET B7= 1
 FLPT     SA2         B1               .SET X2 = NEXT CDC NUMBER
          BX4         X2               .COPY NUMBER TO X4
          AX4         59               .FILL X4 WITH SIGN BIT
          BX7         X4-X2            .COMPLEMENT TO X7 IF NEGATIVE
          ZR          X7,RESTORE       .ALLOWS FOR CDC +-0.
          IR          X2,INRANGE       .TEST IF CDC NUMBER IN RANGE (760
          SA2         LIMIT            .LOAD LARGEST NORD FP NUMBER
          ZR          ADDSIGN          .ADD SIGN AND RETURN THIS VALUE
 INRANGE  UX7         B5,X7            .UNPACK +VE MANT TO X7,EXP TO B5
          AX7         25               .REDUCE MANT.TO 23 BITS
          BX7         X7*X5            .REMOVE BIT 23
          SX6         B5+B3            .ADD NORD BIAS (2**8) +48 FOR DEC
          BX3         -X1*X6           .MASK OUT RIGHT 9 BITS.
          NZ          X3,OUTRANGE      .TEST FOR NON-ZERO IE NORD EXP OV
          LX6         22               .POSITION AS NORD EXPONENT
          BX2         X6+X7            .OR MANT AND EXP INTO X7
          ZR          ADDSIGN          .JUMP ROUND OUTRANGE
 OUTRANGE SA2         LIMIT            .SET LARGEST NORD NUMBER
          PL          X3,ADDSIGN       .BRANCH TO ADD SIGN IF EXP +VEGN
          AX2         30               .SHIFT TO BE INTEGER 1
 ADDSIGN  BX7         X4*X0            .SET NORD SIGN BIT
          BX7         X7+X2            .OR IN SIGN BIT
 RESTORE  SA7         B1               .STORE CONVERTED NUMBER (-0. TO +
          SB1         B1+B7            .ADD 1 TO FIRST ADDRESS
          LT          B1,B2,FLPT       .INSTACK LOOP TILL ALL CONVERTED
          EQ          SXSND            .RETURN
 LIMIT    DATA        B00000000017777777776 .LARGEST NORD FP NUMBER
          END
