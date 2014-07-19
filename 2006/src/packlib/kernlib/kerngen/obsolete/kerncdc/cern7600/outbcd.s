*
* $Id: outbcd.s,v 1.1.1.1 1996/02/15 17:51:38 mclareni Exp $
*
* $Log: outbcd.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:38  mclareni
* Kernlib
*
*
          IDENT  OUTBCD
*
* CERN PROGLIB# I410    OUTBCD          .VERSION KERNCDC  1.16  820129
*
          SST
          ENTRY  OUTBCD
          EXT GETFIT.             NEW SYSTEM
*         EXT GETFIT$             OLD SYSTEM
*
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
          MX6   1
          BX1   X1+X6
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
