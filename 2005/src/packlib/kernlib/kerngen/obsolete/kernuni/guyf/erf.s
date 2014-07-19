*
* $Id: erf.s,v 1.1.1.1 1996/02/15 17:54:02 mclareni Exp $
*
* $Log: erf.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:02  mclareni
* Kernlib
*
*
. **************************************
.
.     FUNCTION ERF( X )                          *** C 300 ***
.     FUNCTION ERFC( X )
.     FUNCTION FREQ( X )
.     METHOD: HASTING
.     SOURCE = COPENHAG
.
. **************************************
 
          AXR$
          FORT$
$(0)
CONS1     + 0.7071067814               . 1./SQRT(2.)
XLIM      + 12.0
CONS3      + 2.0
CONS2
COEF1     + 1.0
COEF2     + 0.070523078
COEF3     + 0.042282012
COEF4     + 0.0092705272
COEF5     + 0.0001520143
COEF6     + 0.0002765672
COEF7     + 0.0000430638
 
$(1)
FREQ*
#include "ftnors1.inc"
          L         A0,0,XARG
          L         A4,CONS1
          FM        A4,0,A0           . X / SQRT( 2. )
          L,U       A5,1
          J         ERF2
ERFC*
          LN,U      A5,1               . SET FLAG NEGATIVE
          J         ERF1
ERF*
          L,U       A5,0               . FLAG = 0
ERF1
#include "ftnors1.inc"
          L         A0,0,XARG
          L         A4,0,A0           . X
ERF2
#include "spyuse.inc"
* Ignoring t=pass
          LM        A2,A4              . /X/
          TG        A2,XLIM            . TEST  /X/  LARGE
          J         ERF7
           S         A2,R1
          FM        A2,COEF7
          FA        A2,COEF6
          FM        A2,R1
          FA        A2,COEF5
          FM        A2,R1
          L         A0,COEF1
          FA        A2,COEF4
          FM        A2,R1
          FA        A2,COEF3
          FM        A2,R1
          FA        A2,COEF2
          FM        A2,R1
          FA        A2,A0
          FD        A0,A2
          FM        A0,A0
          FM        A0,A0
          FM        A0,A0
          FM        A0,A0
ERF3
          JZ        A5,ERF4            . JUMP IF ERF
          JN        A5,ERF6            . JUMP IF ERFC
          FD        A0,CONS3          . T / 2.
          JN        A4,RETUR1
ERF4
          FAN       A0,CONS2
          JN        A4,RETUR1
ERF5      LN        A0,A0
          J         RETUR1
ERF6
          JP        A4,RETUR1
          FAN       A0,CONS3
          J         ERF5
ERF7
          L,U       A0,0
          J         ERF3
          END
#ifdef CERNLIB_TCGEN_ERF
#undef CERNLIB_TCGEN_ERF
#endif
#ifdef CERNLIB_TCGEN_FREQ
#undef CERNLIB_TCGEN_FREQ
#endif
