*
* $Id: datimh.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: datimh.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  01/09/76
          AXR$
 .    CALL DATIMH (ND,NT)    RETURN HOLLERITH DATE + TIME
 .                           ND = 8HDD/MM/YY,   NT = 8HHH.MM.SS
 .                           STRASSBURG, AUG 1976
$(1)
DATIMH*   ER        DATE$
          S         A1,TIME
 .
 .    PROCESS DATE
          L         A1,A0    . A1= MMDDYY
          SSL       A1,24    . A1= 0000MM
          LSSL      A1,6     . A1= 000MM0
          A         A1,DM    . A1= 00/MM/
          L         A3,A0
          SSL       A3,12    . A3= 00MMDD
          LSSL      A3,24    . A3= DD0000
          A         A1,A3    . A1= DD/MM/
          L,H2      A2,0,X11
          LXI,U     A2,1
          S         A1,0,*A2
 .
          LSSL      A0,24    . A0= YY0000
          A         A0,YY    . A0= YY
          S         A0,0,*A2
 .
 .    PROCESS TIME
          L         A1,TIME
          L         A0,A1    . A0= HHMMSS
          SSL       A0,24    . A0= 0000HH
          LSSC      A0,24    . A0= HH0000
          A         A0,W1    . A0= HH.00.
          L         A2,A1
          LSSL      A2,12    . A2= MMSS00
          SSL       A2,24    . A2= 0000MM
          LSSL      A2,6     . A2= 000MM0
          A         A0,A2    . A0= HH.MM.
          L,H2      A2,1,X11
          LXI,U     A2,1
          S         A0,0,*A2
 .
          L         A0,A1
          LSSL      A0,24    . A0= SS0000
          A         A0,W2    . A0= SS
          S         A0,0,*A2
          J         3,X11
$(0)
TIME      +         0
DM        +         0000074000074
YY        +         0000005050505
W1        +         0000075000075
W2        +         0000005050505
          END
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
