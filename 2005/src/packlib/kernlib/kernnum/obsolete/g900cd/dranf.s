*
* $Id: dranf.s,v 1.1.1.1 1996/02/15 17:48:13 mclareni Exp $
*
* $Log: dranf.s,v $
* Revision 1.1.1.1  1996/02/15 17:48:13  mclareni
* Kernlib
*
*
*ADDFILE
*DECK     G900CDC
          IDENT     DRANF
*CALL     BASIC
          EXT       RANDOM.             ADDRESS OF CURRENT SEED
 NAME     MICRO     1,,*DRANF*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
 [1]      DX7       X4*X5               X7=NEW SEED
          SX3       16404B              2**(-48) = 1640 4000 ... 0000B
          NO
          LX3       45                  X3=2**(-48)
          NX6       X7                  X6=DRANF'
          SA7       A4                  STORE NEW SEED
          FX7       X6*X3               X7=DRANF''
          EXIT      "NAME"              DRANF(DUMMY)
          SA4       RANDOM.             X4=OLD SEED
          SA5       A4+1                X5=MULT
          JP        [1]
          END
