*
* $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:52:45 mclareni Exp $
*
* $Log: jbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:45  mclareni
* Kernlib
*
*
          IDENT     JBYT
*
* CERN PROGLIB# M421    JBYT            .VERSION KERNCRY  1.00  830612
* ORIG. 12/06/83 H.LIPPS, CERN
*
JBYT      ENTER     NP=3                (A,LA,NBITS)
          ARGADD    A5,1                A5=(A)
          ARGADD    A6,2                A6=(LA)
          ARGADD    A7,3                A7=(NBITS)
          S5        ,A5                 S5=A
          A6        ,A6                 A6=LA
          A7        ,A7                 A7=NBITS
          A6        A6-1                A6=LA-1
          S5        S5>A6               S5=A*2**(1-LA)
          S7        -1                  S7=FFFF FFFF FFFF FFFF
          S7        S7<A7               S7=FFFF FFFF FFFF FF00
          S1        #S7&S5              S1=MOD(S5,2**NBITS)
          EXIT
          END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
