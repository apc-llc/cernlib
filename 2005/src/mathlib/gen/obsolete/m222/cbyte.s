*
* $Id: cbyte.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: cbyte.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT       CBYTE
****      M222      VERSION    26/10/71 CBYTE       288
*
* THIS ROUTINE CONVERTS 8-BIT BYTES INTO 6-BIT DISPLAY CODE.
* THE FORTRAN CALLING STATEMENT IS
*     CALL CBYTE (ARRAY, NCHAR, CODE)
*                   ARRAY CONTAINS ON INPUT 8-BIT-BYTES, ON OUTPUT DISPL
*                   CHARACTERS
*                   NCHAR IS THE NUMBER OF 8-BIT-BYTES TO BE CONVERTED
*                   CODE IS EITHER 6HEBCDIC OR 7HUSASCII OR 5HASCII
*                   OR 5HTELEX OR 7HIBM1401
* THE FOLLOWING REGISTERS KEEP THEIR DESTINATION THROUGHOUT THE SUBROUTI
*        A0  =  STORE WORD ADDRESS
*        B1  =  GET WORD ADDRESS
*        B2  =  NUMBER OF CHARACTERS TO BE CONVERTED
*        B3 = 0  EBCDIC
*        B3 = 1 USASCII
*        B3 = 2 ASCII
*        B3 = 3 TELEX FS
*        B3 = 4 TELEX LS
*        B4  =  GET CHARACTER COUNT GC
*        B7  =  1
*        X0  =  8-BIT MASK LEFT-ADJUSTED
*        X1  =  WORD TO BE CONVERTED
*        X6  =  STORE CHARACTER COUNT SC
*        X7  =  WORD TO BE STORED
*
MACRO     XTEXT
          ENTRY       CBYTE
* SUBROUTINE CBYTE(ARRAY,NCHAR,MODE)
*
 NCH      BSS         1
 SW       BSS         1                STORE WORD COUNT
 FLAG     BSS         1   .FLAG=+1 FOR BYTES 1-7.5, FLAG=-1 FOR BYTES 7.
 TABLE    BSSZ        1
 FIRST    BSSZ        1
 ASC      DATA        5HASCII
 EB       DATA        6HEBCDIC
 IBM      DATA        7HIBM1401
 TEL      DATA        5HTELEX
 USA      DATA        7HUSASCII
*
* USASCII TABLE. THE NINTH COLUMN OF THE FIRST TEN WORDS CONTAINS A SHIF
* COLUMN COUNT IS     9.8.7.6.5.4.3.2.1.0.
*   CODE CONVERSION TABLE CORRECTIONS TO CONVERT ALL NON PRINTABLE
*  LOWER CASE CHARACTERS TO 00 INSTEAD OF THEIR UPPER CASE EQUIVALENT
 USASCII  DATA        74550000000000000000B      CHARS 007B TO 000B
          DATA        66550000000000000000B      CHARS 017B TO 010B
          DATA        60550000000000000000B      CHARS 027B TO 020B
          DATA        52550000000000000000B      CHARS 037B TO 030B
          DATA        44557067635360646655B      CHARS 047B TO 040B
          DATA        36555057465645475251B      CHARS 057B TO 050B
          DATA        30554241403736353433B      CHARS 067B TO 060B
          DATA        22557173547277004443B      CHARS 077B TO 070B
          DATA        14550706050403020174B      CHARS 107B TO 100B
          DATA        06551716151413121110B      CHARS 117B TO 110B
          DATA        55552726252423222120B      CHARS 127B TO 120B
          DATA        55556576627561323130B      CHARS 137B TO 130B
          DATA        55550706050403020174B      CHARS 147B TO 140B
          DATA        55551716151413121110B      CHARS 157B TO 150B
          DATA        55552726252423222120B      CHARS 167B TO 160B
          DATA        55550076667572323130B      CHARS 177B TO 170B
* COLUMN COUNT IS     9.8.7.6.5.4.3.2.1.0.
 EBCDIC   DATA        55555555555555555555B      CHARS 107B TO 100B
          DATA        55556645517257615555B      CHARS 117B TO 110B
          DATA        55555555555555555567B      CHARS 127B TO 120B
          DATA        55557677524753625555B      CHARS 137B TO 130B
          DATA        55555555555555555046B      CHARS 147B TO 140B
          DATA        55557173656356755555B      CHARS 157B TO 150B
          DATA        55555555555555555555B      CHARS 167B TO 160B
          DATA        55556454707460007455B      CHARS 177B TO 170B
          DATA        55550706050403020155B      CHARS 207B TO 200B
          DATA        55555555555555551110B      CHARS 217B TO 210B
          DATA        55552017161514131255B      CHARS 227B TO 220B
          DATA        55555555555555552221B      CHARS 237B TO 230B
          DATA        55553027262524237655B      CHARS 247B TO 240B
          DATA        55555555555555553231B      CHARS 257B TO 250B
          DATA        55555555555555555555B      CHARS 267B TO 260B
          DATA        55555555555555555555B      CHARS 277B TO 270B
          DATA        55550706050403020172B      CHARS 307B TO 300B
          DATA        55555555555555551110B      CHARS 317B TO 310B
          DATA        55552017161514131266B      CHARS 327B TO 320B
          DATA        55555555555555552221B      CHARS 337B TO 330B
          DATA        55553027262524235575B      CHARS 347B TO 340B
          DATA        55555555555555553231B      CHARS 357B TO 350B
          DATA        55554241403736353433B      CHARS 367B TO 360B
          DATA        55555555555555554443B      CHARS 377B TO 370B
*
 TELEX    DATA        00005756535544004055B
          DATA        00005455334365375200B
          DATA        00005070416447554536B
          DATA        00000051344200773546B
          DATA        00001516105517002455B
          DATA        00002603201107221400B
          DATA        00003006312302043205B
          DATA        00000013212500122701B
*
 IBM1401  DATA        00007755553755433300B
          DATA        00000055415245547535B
          DATA        00005500405144647334B
          DATA        00005556504200463647B
          DATA        00005714240430102000B
          DATA        00005516260632122202B
          DATA        00005515250531112101B
          DATA        00005517270700132303B
          VFD         42/5CCBYTE,18/CBYTE
#include "entftn.inc"
          ENTFTN      CBYTE,3
          SA2         B2               LOAD N
          SX6         B0
          SA3         B3               LOAD FLAG FOR EBCDIC OR USASCII
          SX7         B2
          SA5         FIRST
          SA4         EB
          SB7         1
          SA6         SW
          IX4         X3-X4
          SX6         EBCDIC
          SA7         NCH              SAVE ADDRESS OF THIRD CALL PARM.
          MX0         8                SET UP GET MASK
          NZ          X5,GETLFS
          SB3         B0
          ZR          X4,GO
          SA4         USA
          SX6         USASCII
          SB3         B7
          IX4         X3-X4
          ZR          X4,GO            USASCII
          SA4         ASC
          SB3         B3+B7            FLAG FOR ASCII PAPER TAPE CODE
          IX4         X3-X4
          ZR          X4,GO
          SA4         TEL
          SX6         TELEX
          SB3         B3+B7
          IX4         X4-X3
          ZR          X4,GO
          SA4         IBM
          SX6         IBM1401
          IX4         X4-X3
          ZR          X4,GO
          EQ          B0,B0,CBYTE
*
 GETLFS   SB3         X5
          EQ          B0,B0,START
 GO       SA6         TABLE
 START    SA0         B1               SAVE ADDRESS OF A(1)
          SX5         B0               ZERO THE STORE CHAR COUNT
          BX4         X6               LOAD SW
          SX7         B7
          SB2         X2               B2.= N
          SB4         B0               B4.= GET CHAR COUNT
          SA7         FLAG
          SX7         B0
*
* LOOP ENTERED FOR LOADING NEXT WORD TO BE CONVERTED
 LOOP     SA1         B1               LOAD A(1)
*
* LOOP ENTERED FOR LOADING NEXT CHARACTER TO BE CONVERTED
 LOOP1    SA4         SW               LOAD STORE WORD COUNT
          LX2         B7,X4            2*SW
          BX3         X4
          LX3         3                8*SW
          IX6         X2+X3            10*SW
          IX6         X5+X6            10*SW + SC
          SB6         X6
          GE          B6,B2,FINIS      ALL CHARS CONVERTED
          SB6         10
          SB5         X5
          LT          B5,B6,SKIP       SC .LT. 10
          SX5         B0               RESET STORE CHAR COUNT
          SA7         A0               STORE FULL WORD
          SA0         A0+B7
          SX7         B0               ZERO THE BUFFER WORD
          SX6         X4+B7            AUGMENT STORE WORD COUNT
          SA6         SW               UPDATE STORE WORD COUNT
 SKIP     SB6         7
          LT          B4,B6,NEXT       GC .LT. 7
          SA2         FLAG
          SB4         B0-B7            RESET GET CHAR COUNT
          SB1         B1+B7            AUGMENT GET WORD COUNT
          NG          X2,SECOND
          MX4         4
          LX4         4
          BX3         X1*X4            FIRST 4 BITS OF ROW POINTER
          SA1         B1               LOAD NEXT WORD
          LX4         56
          BX4         X1*X4            LAST 4 BITS OF CHAR
          LX1         4                ADJUST WORD FOR DECODING
          LX4         4
          LX3         4
          BX3         X3+X4            CHAR RIGHT ADJUSTED
          LX3         52               CHAR LEFT ADJUSTED
          SX6         B0-B7            SWITCH FLAG
          SA6         A2
          EQ          B0,B0,CHECKIN
 SECOND   SX6         B7               SWITCH FLAG
          SB4         B0
          SA4         SW
          SA6         A2
          EQ          B0,B0,LOOP
 NEXT     SB5         3
          SX4         B4
          LX4         B5,X4            8*GC
          SB6         X4
          LX3         B6,X1            CHAR LEFT ADJUSTED IN X3
          BX3         X0*X3            X3.= CHAR ONLY
 CHECKIN  NE          B3,B0,CHECK
          MX2         1
          LX2         59
          BX3         X2+X3            CONVERT SMALL INTO BIG LETTERS
 CHECK    SB6         B3-3
          LT          B6,B0,NOFHC      NO 5-HOLE CODE
          SA2         TABLE
          SX4         TELEX
          IX4         X2-X4
          ZR          X4,NOFHC         TELEX CODE
          NZ          X3,JUMP1
          SX3         33B              REPLACE IBM1401 LS BY TELEX FS
          EQ          B0,B0,SHIFT
 JUMP1    LX3         8
          SX4         X3-33B
          NZ          X4,SHIFT
          SX3         37B              REPLACE IBM1401 LS BY TELEX LS
 SHIFT    LX3         52
 NOFHC    SB5         3
          LX3         5                SPLIT CHAR INTO ROW AND COL PTER
          BX4         X0*X3            X4.= COL PTER LEFT ADJUSTED
          BX3         X3-X4            GET ROW POINTER
          LX4         3                X4.= COLUMN POINTER
          SB6         B3-3
          LT          B6,B0,CODE
          SB5         X3-3
          NZ          B5,CODE
          SB5         X4-7
          ZR          B5,LETSH         LETTER SHIFT CHAR FOUND
          SB5         X4-3
          ZR          B5,FIGSH         FIGURE SHIFT CHAR FOUND
 CODE     NZ          B3,ASCII         EBCDIC OR USASCII
          SB5         X3-8             SUBTRACT 8
          LT          B5,B0,BLANK
          EQ          B0,B0,GET
 ASCII    SB5         X3
          EQ          B3,B7,GET
          SB6         3
          EQ          B3,B6,GET
          SB6         B6+B7
          SB5         B5+B6
          EQ          B3,B6,GET
          SX2         B5-B6
          MX3         4
          LX3         4
          BX3         X2*X3
          SB5         X3
          EQ          B0,B0,GET
 BLANK    SA2         USASCII          LOAD BLANK WORD
          EQ          B0,B0,NOW
 GET      SA2         TABLE            LOAD ADDRESS OF TABLE WORD
          SA2         X2+B5            LOAD TABLE WORD
 NOW      SB6         X4+B7            B6.= COLUMN POINTER
          SA4         B6+USASCII       LOAD SHIFT COUNT
          MX3         6
          BX4         X3*X4            GET SHIFT COUNT
          LX4         6
          SB6         X4
          LX2         B6,X2
          SB6         X5               B6.= STORE CHAR COUNT
          BX2         X2*X3            X2.= DISPLAY CHAR
          SA4         B6+USASCII       LOAD SHIFT COUNT
          SB4         B4+B7            AUGMENT GET CHAR COUNT
          BX4         X3*X4            GET SHIFT COUNT
          SX5         X5+B7            AUGMENT STORE CHAR COUNT
          LX4         6
          SB6         X4
          LX2         B6,X2            SHIFT DISPLAY CHAR
          BX7         X2+X7            ADD NEW DISPLAY CHAR
          EQ          B0,B0,LOOP1
 LETSH    SB3         4                LETTER SHIFT FLAG
          SB4         B4+B7            AUGMENT GET CHAR COUNT
          SB2         B2-B7            DECREASE TOTAL CHR COUNT
          EQ          B0,B0,LOOP1
 FIGSH    SB3         3                FIGURE SHIFT FLAG
          SB4         B4+B7
          SB2         B2-B7
          EQ          B0,B0,LOOP1
 FINIS    SA3         NCH
          SA7         A0               STORE LAST WORD
          SX6         B2               NUMBER OF CHARS TO BE RETURNED
          SA6         X3
          SX6         B3               SAVE FS OR LS LEVEL
          SA6         FIRST
          EQ          B0,B0,CBYTE
          END
