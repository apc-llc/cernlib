*
* $Id: wait.s,v 1.1.1.1 1996/04/01 15:03:22 mclareni Exp $
*
* $Log: wait.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:22  mclareni
* Mathlib gen
*
*
          IDENT  WAIT
          ENTRY  WAIT
*...
*...      CE SOUS PROGRAMME PERMET D ATTENDRE
*...      "N" SECONDES.
*...
*...      "N" EST LIMITE A 15 SECONDES
*...
*...      SI N=0 IL N Y A PAS D ATTENTE ..............
*...
*...
*...      CALL WAIT(N)
*...         OU
*...      SX1    ADDRESSE DU TEMPS "N"
*...      RJ     =XWAIT
*...
TRACE     VFD    42/4LWAIT,18/WAIT
SAVA0     BSS    1
WAIT      EQ     *+1S17
          SX6    A0
          SA1    X1
          SA6    SAVA0
          ZR,X1  WAIT              RETURN SI TEMPS NUL
*...      B2=TEMPS A ATTENDRE
          SB2    X1
*...
*...      ON LIMITE LE TEMPS D ATTENTE A 15 SECONDES
*...
          SB4    15                TEMPS MAXIMUN D ATTENTE
          LE     B2,B4,NOLIMIT     SI TEMP SUP A 15,ALORS TEMP=15
          SB2    B4
NOLIMIT   BSS    0
*...
          RJ     TEMP
*...      ON CALCUL LE TEMPS DE FIN D ATTENTE
          SB2    B3+B2
LOOPP     BSS    0
*...      ON ATTEND ENVIRON 1 DEMI SECONDE
          SB3    20
LOOP      BSS    0
          RECALL                   ATTENTE DE 25MS
          SB3    B3-1
          GT     B3,B0,LOOP
          RJ     TEMP
          SB3    B2-B3
          GT     B3,B0,LOOPP       TEMPS D ATTENTE PAS ENCORE ECOULE
          SA1    SAVA0
          SA0    X1
          EQ     WAIT
          SPACE  2
TABTIM    BSS    1
TEMP      EQ     *+1S17
          RTIME  TABTIM
          SA2    TABTIM
          MX0    36
          AX2    12
          BX1    -X0*X2
          SB3    X1
          EQ     TEMP
          END
