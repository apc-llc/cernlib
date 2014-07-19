*
* $Id: fto360.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: fto360.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
* ROUTINE CHANGED FOR FTN COMPILER LEVEL 10 EP-PACKAGE
#if defined(CERNLIB_UNIVAC)
 .        CERN LIBRARY INDEX:  M 430
 . SUBROUTINE FTO360(A,N,MODE) CONVERTS THE N UNIVAC NUMBERS
 .   STARTING AT A TO RIGHT-ADJUSTED, ZERO-FILLED IBM NUMBERS.
 . MODE = 0 FOR FLOATING POINT CONVERSION, MODE = 1 OR 'I' FOR INTEGER.
 . ALTERNATIVE ENTRYPOINT FOR INTEGER CONVERSION: ITO360(A,N).
 . IF MODE IS NOT SPECIFIED, MODE = 0 IS ASSUMED.
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, MAY 1981
 .
          AXR$                        .
$(1),ITO360*                          . ITO360 ENTRY
          SZ        A5                .
          J         $+2               .
FTO360*   LA,U      A5,1              . FTO360 ENTRY
          DL        A1,0,A0           .
          LXI,U     A1,1              . (1,ADDR(A))
          LA        A2,0,A2           .
          ANA,U     A2,1              .
          LR        R1,A2             . REPEAT COUNT
          JZ        A5,INTEG          .
          LA        A3,2,A0           .
          LSSL      A0,9              . # OF ARGUMENTS
          SSL       A0,27             .
          SZ        A4                .
          TE,U      A0,2              .
          LA        A4,0,A3           . FLOATING OR INTEGER CONVERSION ?
          JZ        A4,FLOAT          .
          SSL       A4,27             .
          TNE,U      A4,0106          . MODE='F' ?
          J         FLOAT             .
          TNE,U     A4,0146           . MODE='F' (LOWER CASE) ?
          J         FLOAT             .
INTEG     LA        A3,0,A1           . CONVERSION OF INTEGERS
          JP        A3,$+4            . POSITIVE ?
          LSSL      A3,4              . NO, ADD 1
          AA,U      A3,020            .
          SSL       A3,4              .
          SA        A3,0,*A1          . STORE BACK
          JGD       R1,INTEG          . END OF LOOP
          J         0,X11             . RETURN
FLOAT     LUF       A2,0,A1           . FLOATING POINT CONVERSION
          LMA       A4,A3             . EXP IN A2, ABS IN A4, # IN A5
          LA        A5,A3             .
          DSL       A2,2              . EXP IS POWER OF 16
          SSL       A3,34             .
          JZ        A3,$+4            .
          AA,U      A2,1              . MAKE SHIFTS IF MOD(EXP,4).NE.0
          LNA       A0,A3             .
          SSA       A4,4,A0           .
          AA,U      A2,32             . UNIVAC EXP IS BIASED 0200, IBM IS 64
          JN        A5,$+3            . INSERT SIGN BIT
          AND,U     A2,0177           .
          J         $+2               .
          OR,U      A2,0200           .
          LSSL      A4,9              . SHIFT PARTS TOGETHER
          LDSL      A3,24             .
          SA        A3,0,*A1          . STORE BACK IN IBM-FORMAT
          JGD       R1,FLOAT          . END OF LOOP
          J         0,X11             . RETURN
          END                         .
#endif
