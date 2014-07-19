*
* $Id: intsor.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: intsor.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . CALL INTSOR(IA,A)
             AXR$
$(1)
INTSOR*      L       A0,0,X11      . A0=AD(IA)
             L       A5,*1,X11     . A5=N
             AN,U    A5,1          . A5=N-1
             L       X3,A0         .
             LXI,U   X3,1          . X3=1,AD(IA)
SKOP1        L       A1,X3         .
             A,U     A1,1          . A1=(START AD FOR TESTING)
             L       R1,A5         . SET LOOP COUNTER.
             L       A2,0,X3       . SET UP AND SAVE
             L       A3,A2         . INITIAL VALUE.
             L       X2,X3         . SAVE ADDRESS IN X3.
SKOP3        SLE     A2,0,*A1      . FIND A SMALLER VALUE.
             J       SKOP4         . NONE SMALLER
             AN,U    A1,1          . BACKSPACE A1
             L       X2,A1         . AND SAVE ADDRESS OF SMALLER VALUE.
             L       A2,0,*A1      . RESET A2
             J       SKOP3         . AND CONTINUE TESTING.
SKOP4        S       A2,0,*X3      . SMALLEST TO FIRST PLACE
             S       A3,0,X2       . AND FIRST TO VACANCY.
             AN,U    A5,1          . DECREMENT A5 FOR NEXT TIME ROUND.
             TZ      A5            . ARE WE FINISHED<
             J       SKOP1         . NO - LOOP FOR NEXT.
             J       3,X11         . YES - RETURN.
             END
#ifdef CERNLIB_TCGEN_INTSOR
#undef CERNLIB_TCGEN_INTSOR
#endif
