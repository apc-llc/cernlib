*
* $Id: ucocop.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: ucocop.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . UCOCOP AND UDICOP
           AXR$
$(1)
 . ENTRY UDICOP(A,B,IDO,IW,NA,NB)
UDICOP*   L,U    A5,1       . SET MODE FLAG.
          J      $+2        .
 . ENTRY UCOCOP(A,B,IDO,IW,NA,NB)
UCOCOP*  SZ      A5         . SET MODE FLAG.
         L       A0,0,X11   . PU ADR(A)
         L       A1,1,X11   . PU ADR(B)
         LXI,U   A0,1       . A0=1,ADR(A)
         LXI,U   A1,1       . A1=1,ADR(B)
         L       A2,*2,X11  . A2=IDO
         L       A3,*3,X11  . A3=IW
         TNZ     A2         .
         J       7,X11      . RETURN@@-IDO=0
         TNZ     A3         .
         J       7,X11      . RETURN@@-IW=0
         AN,U    A2,1       . A2=IDO-1
         TNZ     A5         . WHICH FUNCTION<
         J       COCO       . UCOCOP.
         L       A4,A2      . UDICOP - GO BACKWARDS.
         MSI     A4,*4,X11  . GET ADDRESS OF LAST
         A       A4,A3      . WORD IN A
         AN,U    A4,1       .
         A       A0,A4      .
         L       A4,A2      .
         MSI     A4,*5,X11  . GET ADDRESS OF LAST
         A       A4,A3      . WORD IN B
         AN,U    A4,1       .
         A       A1,A4      .
         LN      A4,*4,X11  .
         A       A4,A3      . A4=IW-NA
         LN      A5,*5,X11  .
         A       A5,A3      . A5=IW-NB
         LXI,U   A0,-1      . REVERSE INDEX DIRECTION.
         LXI,U   A1,-1      .
         J       COPYAL     .
COCO     L       A4,*4,X11  .
         AN      A4,A3      . A4=NA-IW
         L       A5,*5,X11  .
         AN      A5,A3      . A5=NB-IW
COPYAL   LR      R3,A2      . R3=IDO-1
         LR      R1,A3      . R1=IW
         BT      A1,0,*A0   . SHIFT 'EM
         A       A0,A4      . UPDATE ADDRESSES FOR
         A       A1,A5      . NEXT BLOCK.
         JGD     R3,$-4     . LOOP FOR NEXT BLOCK.
         J       7,X11      . ALL DONE@-RETURN.
         END
#ifdef CERNLIB_TCGEN_UCOCOP
#undef CERNLIB_TCGEN_UCOCOP
#endif
