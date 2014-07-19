*
* $Id: inbcd.s,v 1.1.1.1 1996/02/15 17:51:38 mclareni Exp $
*
* $Log: inbcd.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:38  mclareni
* Kernlib
*
*
          IDENT  INBCD
*
* CERN PROGLIB# I410    INBCD           .VERSION KERNCDC  1.16  820129
*
          SST
          ENTRY  INBCD
          EXT GETFIT.             NEW SYSTEM
*         EXT GETFIT$             OLD SYSTEM
*
*         NCH = INBCD (LUN,VECT,NCHMAX,NWR)
*
 TRACE    VFD  30/5HINBCD,30/INBCD
 INBCD    DATA 0
          SA2  LUNH            X2= PREVIOUS LFN
          SA5  X1              X5= LUN
          SX6  A1
          SB1  A1              B1= ADR (PAR. LIST)  SAVED
          SA6  SVA1            SAVE START ADR OF PARAMETER LIST
*
*         CHECK IF THIS PARTICULAR FILE WAS THE LAST USED
*         IF FILE IS NOT OPEN, OPEN IT
*
          IX2  X2-X5
          SA4  STOFIT              FIT ADR
          NZ   X2,NOFIT
          BX1  X4              X1= FIT ADR
          FETCH  X1,OC,X3      GET OPEN/CLOSE FLAG
          SX3  X3-1
          NZ   X3,OPEN         JUMP IF FILE NOT OPEN
*
*         GET RECORD ,  RETURN NCH IN X3
*
 NJUMP    SX4  DEXIT           X4= END-OF-DATA EXIT
          SA2  B1+1            X2= ADR OF VECT
          SA3  A2+1                ADR OF MAXCHR
          SA3  X3              X3= MAXCHR
          GETP  X1,X2,X3,X4,,SKIP
          SA1  SVA1                ADR OF PARAMETER LIST
          SB7  1               B7=  1
          SA1  X1+B7
          SB1  B7+B7
          SA4  A1+B1
          SB6  B0-B7           B6= -1
          SB5  X1              B5= ADR (VECT)
          SB4  X4              B4= ADR (NWR)
          SA5  BLANK           X5= ALL BLANKS
          SA4  TENTH           X4= 1/10  WITH SPECIAL NORMALISATION
          ZR   X3,ZEROCH
*
*              CALCULATE  B1=(NW-1), B2= NU, B3=6*NU - 1
*                         NW = NUMBER OF WORDS READ
*                         NU = NUMBER OF CHARACTERS USED IN LAST WORD
          SX7  X3+B6           X7= NCH-1
          PX7  B0,X7           X7= NCH-1   FLOAT
          SX2  10
          FX4  X4*X7           X4= (NCH-1)/10 = NW-1
          PX2  B0,X2           X2= 10      FLOAT
          SB1  X4              B1= NW-1
          DX2  X2*X4           X2= 10*(NW-1)   FLOAT
          UX2  B0,X2           X2= 10*(NW-1)
          IX2  X3-X2           X2= NCH - 10*(NW-1) = NU
          SB2  X2              B2= NU
          LX2  1                   2*NU
          SB3  X2+B2               3*NU
          SB3  B3+B3               6*NU
          SB3  B3-B7           B3= 6*NU - 1
*
*              BLANK-FILL LAST WORD FOR UNUSED CHARACTERS
          SA1  B5+B1           A1= ADR (LAST WORD OF VECT USED)
          MX0  1
          AX0  B3,X0           X0= MASK OF 6*NU BITS
          BX6  -X0*X5
          BX1   X0*X1
          BX1   X1+X6          X1= LAST WORD WITH BLANK-FILL
          BX7  X1
          SA7  A1                  STORED BACK
*
*              DISCARD TRAILING BLANKS
          MX0  54              X0= MASK LAST CHARACTER
          BX6  -X0*X5          X6= 55B
          IX7  X1-X5           TEST LAST WORD ALL BLANKS
          ZR   X7,ALLBL
          SB3  B3+B7           B3= 6*NU
          LX1  B3,X1               ALIGN LAST CHAR. USED
*
 LOOPBL   BX7  -X0*X1              ISOLATE LAST CHAR
          IX7  X7-X6
          NZ   X7,NOTBL            CHECK BLANK
          SB2  B2-B7               NU = NU-1
          LX1  54                  POSITION PREVIOUS CHAR
          NE   B2,B0,LOOPBL
 ALLBL    EQ   B1,B0,ZEROCH
          SB1  B1-B7           (NW-1) = (NW-1) - 1
          SA1  A1-B7               FETCH PREVIOUS WORD
          IX7  X1-X5
          ZR   X7,ALLBL        CHECK ALL BLANKS
          SB2  10                  NU = 10
          EQ   LOOPBL
*
*              EXIT FOR NCH .NE. 0
 NOTBL    SX2  B1              X2= NW-1
          SX7  10
          PX2  B0,X2
          PX7  B0,X7
          DX7  X2*X7               10*(NW-1)
          SX6  X7+B2               NCH = 10*(NW-1) + NU
          SX7  B1+B7           NWR = (NW-1) + 1
          SA7  B4
          EQ   INBCD
*
*         EXIT FOR NCH = 0
 ZEROCH   BX7  X5
          SA7  B5                  VECT(1)= BLANK
          SX7  B7
          MX6  0               NCH = 0
          SA7  B4              NWR= 1
          EQ   INBCD
*
*-------  END-OF-DATA EXIT.  RESET NCH DEPENDING ON FP
*
 DEXIT    DATA 0
          SA4  SVA1
          SA1  STOFIT
          SA5  X4+3            X5=ADR NWR
          MX7  0
          SA7  X5              RETURN NWR=0
          FETCH X1,FP,X6
          AX6  5
          ZR   X6,EOS
          AX6  1
          ZR   X6,EOP
          SX6  -3
          EQ   INBCD           END-OF-INFORMATION RETURN
 EOP      SX6  -2
          EQ   INBCD           END-OF-PARTITION RETURN
 EOS      SX6  -1
          EQ   INBCD           END-OF-SECTION RETURN
*
*----     FIND THE FIT ADDRESS AND STORE IT IN STOFIT
*
 NOFIT    BX6  X5
          SA6  LUNH            STORE LUN
          MX6   1
          BX1   X1+X6
+RJGETFIT
-         VFD  30/TRACE
          BX6  X1
          SA2  SVA1
          SA6  STOFIT          STORE FIT ADDRESS
          FETCH X1,OC,X3
          SX3  X3-1
          SB1  X2                  B1 RESTORED
          ZR   X3,NJUMP
*
*         OPEN THE FILE
*
 OPEN     OPENM  X1,I-O
          SA4  SVA1
          SB1  X4              B1= ADR (PAR.LIST) RESTORED
          EQ   B0,B0,NJUMP
*
 SVA1     DATA 0
 STOFIT   DATA 1L1
 LUNH     DATA 1L2
 TENTH    DATA O17170631463146314632
 BLANK    DATA 10H
          END
