*
* $Id: demand.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: demand.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 . LOGICAL FUNCTION DEMAND(IDUMMY)
 . FUNCTION VALUE IS .TRUE. IF RUN IS IN DEMAND
 . CODED BY B.S.NILSSON, NBI, COPENHAGEN
 . THIS IS A  F T N  VERSION FROM 1979-04-24
 .
          AXR$                        .
$(1),DEMAND*  LXI,U A1,PCTBD$         . BDI OF PCT
          LA,U      A2,RPCTA$         .
          LDJ       A1,$+1            . SWITCH TO PCT
          LA,S2     A3,030,A2         . WORD 030 FROM PCT
          LDJ       A1,$+1            . SWITCH BANK BACK
          LA,U      A0,1              . SET .TRUE. IN A0
          TE,U      A3,4              . IS VALUE 4?
          SZ        A0                . NO, NOT DEMAND
          J         0,X11             . RETURN
          END                         .
#endif
