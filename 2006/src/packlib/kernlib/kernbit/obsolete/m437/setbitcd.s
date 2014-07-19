*
* $Id: setbitcd.s,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: setbitcd.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
#if (defined(CERNLIB_CDC))&&(!defined(CERNLIB_FORTRAN))
          IDENT     SETBIT
          ENTRY     SETBIT
* SUBROUTINE SETBIT(N,A,L)
* PUTS L (0 OR 1)  IN BIT N OF WORD STRING A
*  N.B. BITS ARE NUMBERED 1,2,3... ETC FROM LEFT TO RIGHT
* AUTHOR H.GROTE CERN/DD VERSION 2 DATE 81/01/31
          VFD       36/6HSETBIT,24/SETBIT
 DUMP     DATA      0
 SETBIT   DATA      0
          SX6       A0
          SA6       DUMP
          SA5    A1                *N TO X5 (*N MEANS ADDRESS OF N, ETC.
          SX4    74B               NO. OF BITS
          SX0    1B
          SA3    A1+1B             *A TO X3
          SA2    X5                N TO X2
          PX7    B0,X4
          NX5    B0,X7             FLOAT(60) TO X5
          IX7    X2-X0             N-1 TO X7
          PX2    B0,X7             FLOAT(N-1) TO X2
          FX1    X2/X5             (N-1)/60 (FLOAT) TO X1
          UX5    B2,X1
          LX2    B2,X5             INT. PART TO X2
          IX6    X3+X2             *A(M) TO X6
          DX5    X2*X4             60*INT. PART
          IX7    X7-X5             MOD(N-1,60) TO X7
          SA2    X6                A(M) TO X2, *A(M) TO A2
          SB2    X7+1B             LEFT SHIFT TO MAKE BIT RIGHT ADJ.
          LX3    B2,X2             SHIFT A(M) LEFT
          SB1    X4                60 TO B1
          SA4    A1+2B             *L TO X4
          SA4    X4                L TO X4
          MX7    73B               59 BIT MASK
          BX5    X0*X4             RIGHTMOST BIT OF L
          SB3    B1-B2             RETURN SHIFT
          BX1    X7*X3             59 LEFT BITS OF SHIFTED A(M)
          BX2    X1+X5             OR WITH BIT
          LX6    B3,X2             SHIFT BACK
          SA6    A2                STORE IN A(M)
          SA1       DUMP
          SA0       X1
          JP        SETBIT
          END
#endif
