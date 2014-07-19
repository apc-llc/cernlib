*
* $Id: mxuty.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: mxuty.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   MXUTY
;#
; CERN PROGLIB# F110    MXUTY           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (MXUTY)
       HRRZI      ,ACSAV
       BLT        ,ACSAV+17             ;   SUBROUTINE MXUTY (A,I)
       SKIPG   A01,@1(A16)              ;   IF (I.EQ.0) RETURN
       JRST       ,END                  ;   J = 1
       SOS        ,A01                  ;   IF (I.EQ.1) GO TO 2
       SETZM      ,A02                  ;   A(J) = 1.
       MOVSI   A05,201400               ;   J = J+1
       JUMPE   A01,LOOPE                ;   DO 1 L=1,I
       MOVEI   A03,1                    ;   A(J) = 0.
       MOVEI   A04,@0(A16)              ;1  J = J+1
       MOVE       ,[XWD LOOPB+1,6]      ;   RETURN
       BLT        ,17                   ;   END
       HRRM    A04,LOOP                 ;INSERT ADDRESSES
       HRRM    A04,LOOP1
       SETZM      ,A04
LOOPB: JRST       ,6
       PHASE   6
LOOP:  MOVEM   A05,(A02)                ;A(J) = 1.
       AOS        ,A02                  ;J = J+1
LOOP1: SETZM      ,(A02)                ;A(J) = 0.
       AOS        ,A02                  ;END OF INNER LOOP
       CAMGE   A04,A01
       AOJA    A04,LOOP1
       SETZM      ,A04
       CAMGE   A03,A01                  ;END OF OUTER LOOP
       AOJA    A03,LOOP
LOOP2: JRST       ,LOOPE
       DEPHASE
LOOPE: MOVE    A16,ACSAV+16             ;A(J) = 1.
       ADDI    A02,@0(A16)
       MOVEM   A05,(A02)
END:   HRLZI      ,ACSAV                ;RESTORE AC'S , RETURN
       BLT        ,17
        GOODBY
ACSAV: BLOCK   20
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_MXUTY
#undef CERNLIB_TCGEN_MXUTY
#endif
