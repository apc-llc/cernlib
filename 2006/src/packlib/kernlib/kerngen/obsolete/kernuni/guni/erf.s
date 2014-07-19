*
* $Id: erf.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: erf.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTION ERF
 . FUNCTION ERFC(X)
 . FUNCTION FREQ(X)
            AXR$
$(1)
 . ENTRY ERF
ERF*        SZ        A5           . SET FLAG FOR ERF:
            L         A4,*0,X11    . A4=X
            DS        A6,REGS      . SAVE REGISTERS.
            LM        A6,A4        . A6=/X/
            L         A7,KONS      . A7=SIGN=+1.0
            TP        A4           .
            L         A7,(-1.0)    . X 0@A7=-1.0
            TG        A6,(12.0)    .
            J         ERF1         . GO TO APPROX X V.LARGE
            L         A2,KONS+6    . A2=KONS(7)
            FM        A2,A6        . A2=KONS(7)*/X/
            L,U       R2,4         . SET UP FOR LOOP
            LXM       A1,KONSAD    .
            LXI,U     A1,-1        . A1=-1,ADR(KONS(6)
ERF2        FA        A2,0,*A1     .
            FM        A2,A6        . A2=(A2+KONS(I))*/X/
            JGD       R2,ERF2      . LOOP FOR NEXT TERM.
            FA        A2,0,A1      . ADD 1.0
            L         A3,KONS      . A3=1.0
            FD        A3,A2        . AND GET RECIPROCAL
            L         A2,A3        . GET POWER 16
            FM        A3,A2        . A3=S**2
            L         A2,A3        .
            FM        A3,A2        . A3=S**4
            L         A2,A3        .
            FM        A3,A2        . A3=S**8
            L         A2,A3        .
            FM        A3,A2        . A3=S**16
            L         A2,KONS      .
            FAN       A2,A3        . A2=1.0-S**16
ERF3        FM        A2,A7        .
            L         A0,A2        . ERF=A0=A2 FOR RETURN.
            DL        A6,REGS      . RESTORE REGISTERS.
            TNZ       A5           . LOOK AT FLAG FOR RIGHT RETURN
            J         2,X11        . FLAG=0@RETURN FOR ERF
            TP        A5           .
            J         ERFCR        .
            FA        A2,KONS      . A2=1.0+ERF
            FM        A2,KONS0     .
            L         A0,A2        . ERF=0.5*(1.0+ERF)
            J         2,X11        . RETURN FROM FREQ.
ERFCR       L         A3,KONS      .
            FAN       A3,A2        .
            L         A0,A3        . ERFC=1.0-ERF
            J         2,X11        . RETURN FROM ERFC.
 . ENTRY ERFC
ERFC*       LN        A5,KONS      . SET FLAG NEGATIVE
            J         ERF+1        . AND CARRY ON.
 . ENTRY FREQ
FREQ*       L         A4,*0,X11    . A4=X
            FM        A4,KONSF     . A4=X/SQRT(2)
            L,U       A5,1         . SET FLAG
            J         ERF+2        . AND CARRY ON
ERF1        L         A2,KONS      . HERE IF /X/ LARGE
            J         ERF3         . A2=1.0
$(2)
KONS0       +         0.5
KONS        +         1.0
            +         0.070523078
            +         0.042282012
            +         0.0092705272
            +         0.0001520143
            +         0.0002765672
            +         0.0000430638
KONSAD      +         $-2
KONSF       +         0.70710678   . 1/SQRT(2)
REGS        RES       2
            END
#ifdef CERNLIB_TCGEN_ERF
#undef CERNLIB_TCGEN_ERF
#endif
#ifdef CERNLIB_TCGEN_FREQ
#undef CERNLIB_TCGEN_FREQ
#endif
