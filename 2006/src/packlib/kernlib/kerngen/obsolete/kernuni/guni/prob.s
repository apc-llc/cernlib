*
* $Id: prob.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: prob.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTION PROB(CHI2,N)
            AXR$
$(1)
PROB*       DS       A6,REGS          . SAVE REGISTERS
            DS       A8,REGS+2        .
            DS       A10,REGS+5       .
            S        A12,REGS+7       .
            SZ       A0               . FOR BLANK RETURNS.
            SX       X11,REGS+4       .
            L        A3,*0,X11        . A3=CHI2
            TNZ      A3               .
            J        PROB13           . SPECIAL EXIT IF CHI2=0.0
            L        A4,*1,X11        . A4=N
            S        A3,CHI2          . SAVE CHI2.
            S        A4,N             . AND N
            TZ       A4               .
            TP       A4               .
            J        PROB12           . N =0@RETURN
            TP       A3               .
            J        PROB12           . CHI2 0@RETURN
            TG       A4,KONS1         .
            J        PROB7            . N:100
            TG       A3,KONS2         .
            J        PROB12           . CHI2:1483.32@RETURN
            L        A1,(-0.5)        .
            FM       A1,A3            .
            S        A1,EMYO2         . EMYO2=-0.5*CHI2
            LMJ      X11,EXP          .
            +        EMYO2            .
            +        $-2-PROB,0       .
            S        A0,EMYO2         . EMYO2=EXP(-0.5*CHI2)
            L        A1,(1.0)         . A1=SUM=1.0
            L        A11,(1.0)        . A11=TERM=1.0
            L        A3,N             .
            SSL      A3,1             . DIVIDE BY 2
            S        A3,M             . M=N/2
            LSSC     A3,1             . MULTIPLY BY 2
            TE       A3,N             .
            J        PROB4            . N IS ODD
            L        A3,M             . N IS EVEN
            TNE,U    A3,1             .
            J        PROB3            . M=1
            L,U      A10,1            . A10=I-1=1
            L        A8,(0.5)         . PRELOAD A8=0.5*CHI2
            FM       A8,CHI2          .
            L,XU     A6,155           . PRELOAD FOR FLOAT OP.
PROB2       LCF      A6,A10           . A7=FLOAT(I-1)
            FM       A11,A8           .
            FD       A11,A7           . A11=TERM=TERM*0.5*CHI2/FI
            FA       A1,A11           . SUM=SUM+TERM
            A,U      A10,1            . INCREMENT I-1
            TE       A10,M            .
            J        PROB2            . LOOP FOR NEXT TIME ROUND
PROB3       L        A0,EMYO2         . HERE IF N=1
            FM       A0,A1            .
            J        PROB12           . RETURN
PROB4       LMJ      X11,SQRT         . HERE IF N IS EVEN
            +        CHI2             .
            +        $-2-PROB,0       .
            S        A0,SRTY          .
            LMJ      X11,FREQ         .
            +        SRTY             .
            +        $-2-PROB,0       .
            L        A1,(1.0)         .
            FAN      A1,A0            .
            FM       A1,(2.0)         .
            L,U      A2,1             .
            TE       A2,N             .
            J        $+3              .
            L        A0,A1            . PROB=VALUE
            J        PROB12           . RETURN
            L        A6,A1            .
            L        A0,KONS3         .
            FM       A0,SRTY          .
            FM       A0,EMYO2         . A0=.7978846*SRTY*EMYO2
            L,U      A2,3             .
            L        A11,(1.0)        . A11=SUM=1.0
            L        A7,(1.0)         . A7=TERM=1.0
            TNE      A2,N             .
            J        PROB6            . N=3
            L        A3,M             .
            AN,U     A3,1             . A3=K=M-1
            L,U      A2,1             . A2=I=1
            L,XU     A4,155           . PRELOAD A4 FOR FLOAT OP.
PROB5       LCF      A4,A2            . A5=FLOAT(I)=FI
            L        A9,(2.0)         .
            FM       A9,A5            .
            FM       A7,CHI2          .
            FA       A9,(1.0)         .
            FD       A7,A9            .
            FA       A11,A7           . SUM=SUM+TERM
            A,U      A2,1             . INCREMENT I
            TG       A3,A2            .
            J        PROB5            .
PROB6       FM       A0,A11           . N=3
            FA       A0,A6            .
            J        PROB12           . RETURN
PROB7       L,XU     A1,155           . PRELOAD A1 FOR FLOAT OP.
            LCF      A1,A4            . A2=FLOAT(N)
            L        A4,(1.0)         .
            FD       A4,A2            . A4=ANU=1./FLOAT(N)
            L        A5,A4            .
            L        A6,CHI2          .
            FD       A5,(4.5)         . A5=AN9=ANU/4.5
            FM       A6,A4            .
            S        A5,EMYO2         . SAVE RESULT
            S        A6,N             . SAVE RESULT
            LMJ      X11,NEXP6$       .
            +        N                . A0=(A6)**KONS4
            +        KONS4            .
            +        $-3-PROB,0       .
            FA       A0,EMYO2         .
            FAN      A0,(1.0)         .
            S        A0,CHI2          .
            LMJ      X11,SQRT         .
            +        EMYO2            .
            +        $-2-PROB         .
            L        A1,CHI2          .
            FD       A1,A0            .
            S        A1,CHI2          .
            LMJ      X11,FREQ         .
            +        CHI2             .
            +        $-2-PROB,0       .
            L        A1,(1.0)         .
            FAN      A1,A0            .
            L        A0,A1            .
PROB12      DL       A6,REGS          . RETURN SEQUENCE.
            DL       A8,REGS+2        . RESTORE REGISTERS.
            DL       A10,REGS+5       .
            L        A12,REGS+7       .
            LX       X11,REGS+4       .
            J        3,X11            . RETURN
PROB13      L        A0,(1.0)         . CHI2=0.0@@SET PROB=1.0
            J        PROB12           . AND EXIT.
$(2)
REGS        RES      8
KONS1       +        100
KONS2       +        1483.32
KONS3       +        0.7978846
KONS4       +        0.3333333333
CHI2        +        0
N           +        0
EMYO2       +        0
M           +        0
SRTY        +        0
            END
#ifdef CERNLIB_TCGEN_PROB
#undef CERNLIB_TCGEN_PROB
#endif
