*
* $Id: ubunch.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: ubunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
UBUNCH   CSECT
*
* CERN PROGLIB# M409    UBUNCH          .VERSION KERNIBM  0.1   721010
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UBUNCH   AMODE ANY
UBUNCH   RMODE ANY
#endif
         ENTRY UBLOW
         BC    15,12(15)           (BLOWNIS REAL*4)
         DC    X'7'                (BUNCH IS REAL*1)
         DC    CL7'UBUNCH '
         STM   2,11,28(13)
         BALR  2,0
         USING *,2
         LM    3,5,0(1)            BLOWN,BUNCH,N IN R3,R4,R5
         LA    6,1                 J=1
         LA    7,4                 K=4
         SR    3,7                 R3=BLOWN-K
         SR    4,6                 R4=BUNCH-J
         L     5,0(5)              R5=N
         SR    11,11
         CR    5,11
         BNH   END
         LR     11,5
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
END      LM    2,11,28(13)
         MVI   12(13),X'FF'
         BCR   15,14               RETURN
UBLOW    BC    15,10(15)           ENTRY UBLOW(BUNCH,BLOWN,N)
         DC    X'5'
         DC    CL5'UBLOW'
         STM   2,9,28(13)
         BALR  2,0
         USING *,2
         LM    3,5,0(1)            BUNCH,BLOWN,N IN R3,R4,R5
         LA    6,1                 J=1
         LA    7,4                 K=4
         SR    3,6                 R3=BUNCH-J
         SR    4,7                 R4=BLOWN-K
         L     5,0(5)              R5=N
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
END1     LM    2,9,28(13)
         MVI   12(13),X'FF'
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
