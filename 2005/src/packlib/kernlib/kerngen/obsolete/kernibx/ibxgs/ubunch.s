*
* $Id: ubunch.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: ubunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UBUNCH   CSECT
*
* CERN PROGLIB# M409    UBUNCH          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _ubunch_
         ENTRY _ublow_
_ubunch_ STM   2,12,16(13)
         LR    3,0                 AIX: address of arg1 = BLOWN
         LR    4,1                 AIX: address of arg2 = BUNCH
         L     5,0(,2)             AIX: value of arg3 = N
         LA    6,1                 J=1
         LA    7,4                 K=4
         SR    3,7                 R3=BLOWN-K
         SR    4,6                 R4=BUNCH-J
         SR    11,11
         CR    5,11
         BNH   END
         LR    11,5
         SR    10,10
         DR    10,7
         SR    11,11
LOOP1    L     9,0(3,7)            R9=BLOWN(K)
         SLDL  8,8
         STC   8,0(4,6)            BUNCH(J)=BLOWN(K)
         LA    6,1(6)              J=J+1
         LA    7,4(7)              K=K+4
         BCT   5,LOOP1             IF R5 NOT 1 R5=R5-1 , GO TO LOOP1
         CR    10,11
         BE    END
         LA    5,4
         SR    5,10
         L     9,BLANKS
LOOP3    STC   9,0(4,6)
         LA    6,1(6)
         BCT   5,LOOP3
END      LM    2,12,16(13)
         BCR   15,14               RETURN
         USING *,15
_ublow_  STM   2,12,16(13)
         LR    3,0                 AIX: address of arg1 = BUNCH
         LR    4,1                 AIX: address of arg2 = BLOWN
         L     5,0(,2)             AIX: value of arg3 = N
         LA    6,1                 J=1
         LA    7,4                 K=4
         SR    3,6                 R3=BUNCH-J
         SR    4,7                 R4=BLOWN-K
         SR    9,9
         CR    5,9
         BNH   END1
LOOP2    L     9,BLANKS
         IC    8,0(3,6)            R8=BUNCH(J)
         SRDL  8,8
         ST    9,0(4,7)
         LA    6,1(6)              J=J+1
         LA    7,4(7)              K=K+4
         BCT   5,LOOP2             IF R5 NOT 1  R5=R5-1, GO TO LOOP2
END1     LM    2,12,16(13)
         BCR   15,14               RETURN
         CNOP  0,4
BLANKS   DC    CL4'    '
         END
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
