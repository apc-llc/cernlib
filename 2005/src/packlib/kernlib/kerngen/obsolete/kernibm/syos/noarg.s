*
* $Id: noarg.s,v 1.1.1.1 1996/02/15 17:53:22 mclareni Exp $
*
* $Log: noarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:22  mclareni
* Kernlib
*
*
NOARG    CSECT
*
* CERN PROGLIB# Z029    NOARG           .VERSION KERNIBM  1.11  820328
*
*      THIS IS A SUBROUTINE TO DETERMINE THE NUMBER OF ARGS
*      WHICH IS NOT VERY SAFE IF NO ARGS ARE SUPPLIED
*      CALLING SEQUENCE : "CALL NOARG(N)"
*
*      AUTHOR: P.BREITENLOHNER, MAX-PLANCK-INSTITUT FUER PHYSIK
*      FOEHRINGER RING 6, D-8 MUENCHEN 40.                14. FEB 1975
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
NOARG    AMODE ANY
NOARG    RMODE ANY
#endif
         USING *,15
         ST    2,28(,13)
         L     2,4(,13)           GET PREVIOUS SAVE AREA
         L     2,24(,2)           GET PREVIOUS PARM POINTER
         LA    2,0(,2)            STRIP OFF HIGH BYTE
         LTR   0,2                SAVE IT
         BZ    EXIT0              DEFINITELY NO ARGS
LOOP     TM    0(2),X'80'         IS THIS LAST ARG ?
         LA    2,4(,2)            STEP TO NEXT ARG
         BNO   LOOP               LOOP IF NOT YET LAST
         SR    2,0                4*NUMBER
         SRA   2,2                NUMBER
         LR    0,2
EXIT0    L     2,0(,1)            LOC TO STORE RESULT
         ST    0,0(,2)
         L     2,28(,13)
         BR    14
         END
#ifdef CERNLIB_TCGEN_NOARG
#undef CERNLIB_TCGEN_NOARG
#endif
