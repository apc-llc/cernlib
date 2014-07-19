*
* $Id: sbit0.s,v 1.1.1.1 1996/02/15 17:52:46 mclareni Exp $
*
* $Log: sbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:46  mclareni
* Kernlib
*
*
          IDENT     SBIT0
*
* CERN PROGLIB# M421    SBIT0           .VERSION KERNCRY  1.00  830612
* ORIG. 12/06/83 H.LIPPS, CERN
*
SBIT0     ENTER     NP=2                (X,LX)
          ARGADD    A6,1                A6=(X)
          ARGADD    A7,2                A7=(LX)
          S6        ,A6                 S6=X
          A7        ,A7                 A7=LX
          S7        1                   S7=1
          A7        A7-1                A7=LX-1
          S7        S7<A7               S7=1*2**(LX-1)
          S1        #S7&S6              CLEAR BIT LX IN S6
          ,A6       S1                  X=S1
          EXIT
          END
#ifdef CERNLIB_TCGEN_SBIT0
#undef CERNLIB_TCGEN_SBIT0
#endif
