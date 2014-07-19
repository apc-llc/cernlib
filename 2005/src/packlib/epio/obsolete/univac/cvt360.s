*
* $Id: cvt360.s,v 1.1.1.1 1996/03/08 15:21:59 mclareni Exp $
*
* $Log: cvt360.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:59  mclareni
* Epio
*
*
* ROUTINE CHANGED FOR FTN COMPILER LEVEL 10 EP-PACKAGE
#if defined(CERNLIB_UNIVAC)
 .        CERN LIBRARY INDEX:  M 219
 . SUBROUTINE CVT360(A(I),A(J),MODE) CONVERTS THE RIGHT-ADJUSTED,
 .   ZERO-FILLED IBM NUMBERS A(I)...A(J) TO UNIVAC 1100 NUMBERS.
 . MODE = 0 FOR FLOATING POINT CONVERSION, MODE = 1 FOR INTEGER.
 . IF MODE IS NOT SPECIFIED, MODE = 0 IS ASSUMED.
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, APRIL 1981
 .
          AXR$                        .
$(1),CVT360*
          DL        A1,0,A0           .
          ANA       A2,A1             . REPEAT COUNT
          LXI,U     A1,1              . (1,ADDR(A(I))
          LA        A5,A0             .
          LSSL      A5,9              . # OF ARGUMENTS
          SSL       A5,27             .
          LA        A3,2,A0           .
          SZ        A4                .
          TE,U      A5,2              . TWO ARGUMENTS ?
          LA        A4,0,A3           . NO, LOAD A4=MODE
          JZ        A4,FLOAT          . FLOATING IF MODE=0
INTEG     LA        A4,0,A1           .
          LSSL      A4,4              . SHIFT TO INCLUDE SIGN-BIT
          SSA       A4,4              .
          JP        A4,$+2            . POSITIVE ?
          ANA,U     A4,1              . SUBTRACT ONE FOR TWOS COMPLEMENT
          SA        A4,0,*A1          . STORE INTEGER IN UNIVAC FORMAT
          JGD       A2,INTEG          .
          J         0,X11             . RETURN
FLOAT     LA        A4,0,A1           .
          LDSL      A3,5              . SIGN IN A3
          DSL       A4,29             . SHIFT MANTISSA TO A5
          SSL       A5,8              . MANTISSA
          LSSL      A4,2              .
          ANA,U     A4,129            . EXPONENT
          LCF       A4,A5             . CONVERT TO FLOATING
          JNB       A3,$+2            . POSITIVE ?
          LNA       A5,A5             . NO, NEGATIVE
          SA        A5,0,*A1          . STORE UNIVAC FLOATING NUMBER
          JGD       A2,FLOAT          . END OF LOOP
          J         0,X11             . RETURN
          END                         .
#endif
