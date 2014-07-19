*
* $Id: omsort.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: omsort.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . SUBROUTINE OMSORT(A,N)
          AXR$
$(1)
OMSORT*   L      A0,0,X11     .
          AN,U   A0,2         .
          LXI,U  A0,1         . A0=1,ADR(A)-2
          L      A4,*1,X11    .
          AN,U   A4,2         . A4=N-2
          L      R3,A4        . R3=N-2
          LXI,U  A1,-1        .
OMSOR1    L      A2,3,A0      . PU SECOND NUMB.
          TLE    A2,2,*A0     . AND COMPARE WITH FIRST.
          J      $+3          . WRONG ORDER.
          JGD    R3,OMSOR1    . CORRECT ORDER - CONTINUE.
          J      3,X11        . RETURN.
          L      A3,1,A0      . EXCHANGE POSITIONS.
          S      A2,1,A0      .
          S      A3,2,A0      .
          LXM    A1,A0        . SET UP TO WALK BACK.
          LN     A5,R3        . A5=-R3
          A      A5,A4        . A5=N-2-R3
          TNZ    A5           . IF A5=0 WE ARE ALREADY THERE.
          J      OMSOR1+3     . RETURN TO FORWARD LOOP.
          AN,U   A5,1         .
          L      R1,A5        . R1=N-2-R3-1
          L      A2,1,A1      . PU SECOND NUMBER.
OMSOR2    TG     A2,0,*A1     . COMPARE WITH PREVIOUS.
          J      OMSOR1+3     . RETURN TO FORWARD LOOP.
          L      A3,1,A1      . EXCHANGE NUMBERS.
          S      A2,1,A1      .
          S      A3,2,A1      .
          JGD    R1,OMSOR2    . TAKE ANOTHER STEP BACK.
          J      OMSOR1+3     . WE HAVE REACHED START.
                              . RETURN TO FORWARD LOOP.
          END
