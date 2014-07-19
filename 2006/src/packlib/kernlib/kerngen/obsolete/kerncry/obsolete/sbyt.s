*
* $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:52:46 mclareni Exp $
*
* $Log: sbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:46  mclareni
* Kernlib
*
*
          IDENT     SBYT
*
* CERN PROGLIB# M421    SBYT            .VERSION KERNCRY  1.00  830612
* ORIG. 12/06/83 H.LIPPS, CERN
*
SBYT      ENTER     NP=4                (A,X,LX,NBITS)
          ARGADD    A4,1                A4=(A)
          ARGADD    A5,2                A5=(X)
          ARGADD    A6,3                A6=(LX)
          ARGADD    A7,4                A7=(NBITS)
          S3        -1                  S3=FFFF FFFF FFFF FFFF
          S4        ,A4                 S4=AAAA AAAA AAAA AAAA
          S5        ,A5                 S5=XXXX XXXX XXXX XXXX
          A6        ,A6                 A6=LX
          A7        ,A7                 A7=NBITS
          S3        S3<A7               S3=FFFF FFFF FFFF FF00
          A6        A6-1                A6=LX-1
          S3        #S3                 S3=0000 0000 0000 00FF
          S4        S4<A6               S4=AAAA AAAA AAAA 0000
          S3        S3<A6               S3=0000 0000 00FF 0000
          S5        S4!S5&S3            S5=XXXX XXXX XXAA XXXX
          ,A5       S5
          EXIT
          END
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
