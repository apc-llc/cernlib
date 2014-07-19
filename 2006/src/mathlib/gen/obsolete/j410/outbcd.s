*
* $Id: outbcd.s,v 1.1.1.1 1996/04/01 15:03:18 mclareni Exp $
*
* $Log: outbcd.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:18  mclareni
* Mathlib gen
*
*
#if !defined(CERNLIB_NOSBE)
          IDENT  OUTBCD
          SST
          ENTRY  OUTBCD
*         SUBROUTINE OUTBCD (LUN,VECT,NCHAR,NW)
          EXT GETFIT.             NEW SYSTEM
*         EXT GETFIT$             OLD SYSTEM
*
 TRACE    VFD  36/6HOUTBCD,24/OUTBCD
 OUTBCD   DATA 0
          SA2  LUNH            X2= PREVIOUS LFN
          SA5  X1              X5= LUN
          SB1  A1              B1= ADR (PAR. LIST)  SAVED
*
*         CHECK IF THIS PARTICULAR FILE WAS THE LAST USED
*         IF FILE IS NOT OPEN, OPEN IT
*
          IX2  X2-X5
          SA4  STOFIT              FIT ADR
          NZ   X2,NOFIT
          BX1  X4              X1= FIT ADR
          FETCH  X1,OC,X3          GET OPEN/CLOSE FLAG
          SX3  X3-1
          NZ   X3,OPEN             JUMP IF FILE NOT OPEN
*
*----     READY PUTTING
*
 NJUMP    SB7  1
          SA2  B1+B7           X2= ADR OF VECT
          SA3  A2+B7
          SA4  A3+B7
          SA3  X3              X3=NCHAR
          SA4  X4              X4= NW
          NZ   X3,CHARS
*
*         ACTION FOR NCHAR = 0
*              IF NW=0  BLANK CARD:  SET NCH = 1
          NZ   X4,WORDS
          SX2  BLANKX          X2= ADR OF BLANKX
          SX3  B7              X3= 1
          EQ   PUT
*
*              CONVERT WORDS TO CHARACTERS
 WORDS    SX7  10              X7= 10
          PX4  B0,X4
          PX7  B0,X7
          DX7  X4*X7               NCH = 10*NW
          SX3  X7+B0           X3= NCH
*
 CHARS    NG   X3,EODATA       IF NCHAR.LT.0, WRITE END-OF-DATA
 PUT      PUT  X1,X2,X3
          EQ   OUTBCD
*
*         WRITE APPROPRIATE END-OF-DATA MARKS
*
 EODATA   SX3  X3+B7
          ZR   X3,EOS
          ENDFILE  X1          WRITE END-OF-INFORMATION MARK
          EQ   OUTBCD
 EOS      WEOR X1,0            WRITE END-OF-SECTION MARK
          EQ   OUTBCD
*
*----     FIND THE FIT ADDRESS AND STORE IT IN STOFIT
*
 NOFIT    BX6  X5
          SX7  B1
          SA6  LUNH                STORE LUN
          SA7  SVA1                B1 SAVED
+RJGETFIT
-         VFD  30/TRACE
          BX6  X1
          SA2  SVA1
          SA6  STOFIT              STORE FIT ADDRESS
          FETCH X1,OC,X3
          SX3  X3-1
          SB1  X2                  B1 RESTORED
          ZR   X3,NJUMP
*
*         OPEN THE FILE
*
 OPEN     SX7  B1
          SA7  SVA1                B1 SAVED
          OPENM  X1,I-O,N
          SA4  SVA1
          SB1  X4              B1= ADR (PAR.LIST) RESTORED
          EQ   B0,B0,NJUMP
*
 SVA1     DATA 0
 STOFIT   DATA 1L1
 LUNH     DATA 1L2
 BLANKX   DATA 10H         X
          END
#endif
#if defined(CERNLIB_NOSBE)
          IDENT OUTBCD
 RTZ      EQU 3
 RTS      EQU 8
 RTW      EQU 0
 BTC      EQU 2
 BTI      EQU 1
 BT0      EQU 0         UNBLOCKED
***
***  CHOOSE YOUR OPTIONS...
***
 RECTYP   EQU RTZ
 BLTYP    EQU BTC
 FLENGTH  EQU 136
***
          ENTRY  OUTBCD
          EXT GETFIT.             NEW SYSTEM
*         SUBROUTINE OUTBCD (LUN,VECT,NCHAR,NW)
*
 TRACE    VFD  36/6HOUTBCD,24/OUTBCD
 OUTBCD   DATA 0
          SA2  LUNH            X2= PREVIOUS LFN
          SA5  X1              X5= LUN
          SB1  A1              B1= ADR (PAR. LIST)  SAVED
*
*         CHECK IF THIS PARTICULAR FILE WAS THE LAST USED
*         IF FILE IS NOT OPEN, OPEN IT
*
          IX2  X2-X5
          SA4  STOFIT              FIT ADR
          NZ   X2,NOFIT
          BX1  X4              X1= FIT ADR
          FETCH  X1,OC,X3          GET OPEN/CLOSE FLAG
          SX3  X3-1
          NZ   X3,OPEN             JUMP IF FILE NOT OPEN
*
*----     READY PUTTING
*
 NJUMP    SB7  1
          SA2  B1+B7           X2= ADR OF VECT
          SA3  A2+B7
          SA4  A3+B7
          SA3  X3              X3=NCHAR
          SA4  X4              X4= NW
          NZ   X3,CHARS
*
*         ACTION FOR NCHAR = 0
*              IF NW=0  BLANK CARD:  SET NCH = 1
          NZ   X4,WORDS
          SX2  BLANKX          X2= ADR OF BLANKX
          SX3  B7              X3= 1
          EQ   PUT
*
*              CONVERT WORDS TO CHARACTERS
 WORDS    SX7  10              X7= 10
          PX4  B0,X4
          PX7  B0,X7
          DX7  X4*X7               NCH = 10*NW
          SX3  X7+B0           X3= NCH
*
 CHARS    NG   X3,EODATA       IF NCHAR.LT.0, WRITE END-OF-DATA
 PUT      PUT  X1,X2,X3
          EQ   OUTBCD
*
*         WRITE APPROPRIATE END-OF-DATA MARKS
*
 EODATA   SX3  X3+B7
          ZR   X3,EOS
          ENDFILE  X1          WRITE END-OF-INFORMATION MARK
          EQ   OUTBCD
 EOS      WEOR X1,0            WRITE END-OF-SECTION MARK
          EQ   OUTBCD
*
*----     FIND THE FIT ADDRESS AND STORE IT IN STOFIT
*
 NOFIT    BX6  X5
          SX7  B1
          SA6  LUNH                STORE LUN
          SA7  SVA1                B1 SAVED
+RJGETFIT
-         VFD  30/TRACE
          BX6  X1
          SA2  SVA1
          SA6  STOFIT              STORE FIT ADDRESS
          FETCH X1,OC,X3
          SX3  X3-1
          SB1  X2                  B1 RESTORED
          ZR   X3,NJUMP
*
*         OPEN THE FILE
*
 OPEN     SX7  B1
          SA7  SVA1                B1 SAVED
***  SET REQUESTED  RT , BT , FL  IN FIT.
          SA5 X1+1
          MX0 4
          SX6 RECTYP
          LX0 26
          LX6 22
          BX5 -X0*X5
          BX5 X5+X6
          MX0 3
          SX6 BLTYP
          LX0 31
          LX6 28
          BX5 -X0*X5
          BX6 X5+X6
          SA6 A5
          STORE X1,FL=FLENGTH
          STORE X1,ERL=128             ERROR LIMIT
          OPENM  X1,I-O,N
          SA1 STOFIT
          SA4  SVA1
          SB1  X4              B1= ADR (PAR.LIST) RESTORED
          EQ   B0,B0,NJUMP
*
 SVA1     DATA 0
 STOFIT   DATA 1L1
 LUNH     DATA 1L2
 BLANKX   DATA 10H         X
          END
#endif
