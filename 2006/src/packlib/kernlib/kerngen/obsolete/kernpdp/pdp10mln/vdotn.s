*
* $Id: vdotn.s,v 1.1.1.1 1996/02/15 17:53:39 mclareni Exp $
*
* $Log: vdotn.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:39  mclareni
* Kernlib
*
*
        TITLE   VDOTN
;#
; CERN PROGLIB# F121    VDOTN           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
       EXTERN  SQRT
        HELLO (VDOTN)
       SETZM
       SKIPG      ,@2(A16)
        GOODBY
        JSR     ,VD
               ENTER (SQRT)
       JUMP      2,TEMP
       MOVEM      ,TEMP
       JRST       ,GO
        HELLO (VDOTN2)
       SETZM
       SKIPG      ,@2(A16)
        GOODBY
        JSR     ,VD
       FMPRM   A01,XY
GO:    HRLZI      ,ACSAVE
       BLT        ,7
       MOVE       ,XY
       FDVR       ,TEMP
        GOODBY
VD:     0
       HRRZI      ,ACSAVE
       BLT        ,ACSAVE+7
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,LOOP
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LOOP+1
       MOVEI   A03,1
       SETZM
       SETZM      ,A01
       SETZM      ,A02
LOOP:  MOVE    A04,(A03)
       MOVE    A05,(A03)
       MOVEM   A04,A06
       FMPR    A04,A04
       FADR       ,A04
       FMPR    A06,A05
       FADR    A01,A06
       FMPR    A05,A05
       FADR    A02,A05
       CAMGE   A03,@2(A16)
       AOJA    A03,LOOP
       FMPR       ,A02
       MOVEM      ,TEMP
       MOVEM   A01,XY
        JRST    2,@VD
ACSAVE:BLOCK   10
XY:    0
TEMP:  0
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A06= 6
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VDOTN
#undef CERNLIB_TCGEN_VDOTN
#endif
#ifdef CERNLIB_TCGEN_VDOTN2
#undef CERNLIB_TCGEN_VDOTN2
#endif
