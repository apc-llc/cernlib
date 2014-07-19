*
* $Id: hcdifb.inc,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: hcdifb.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCDIFB_INC
#define CERNLIB_HBOOK_HCDIFB_INC
*
*
* hcdifb.inc
*
*CMZ :  4.19/00 13/04/93  16.36.40  by  Rene Brun
*-- Author :
C
C----------------------------------------------------------------------
C
C   HDIFFB specific common block
C
C----------------------------------------------------------------------
C
C
C Option string: Contains all possible character options, used as input
C                for the option decoding utility HUOPTC. This returns a
C                1-d array of length the number of options with contents
C                one or zero. One indicates that that option was selected.
C                For example, HUOPTC('A','ABC',SELECTED(3)) would return
C                (1,0,0) in SELECTED array. Likewise, 'CB' would return
C                (0,1,1).
C
C                HDIFFB has 12 possible options, and the resulting 1-d
C                array, OPTS(12) is placed in the HDIFFB common block.
C
C----------------------------------------------------------------------
C
      INTEGER NPARMS
      PARAMETER( NPARMS = 12 )
      CHARACTER*(NPARMS) OPTST
      INTEGER OFLOW, UFLOW, SOPTN, COPTN, AOPTN
      INTEGER NORMD,DEBUG,ZEROS,XUNDR,XOVER,YUNDR,YOVER
C  - possible options string
      PARAMETER( OPTST = 'NDOUSCAZRLTB' )
C     - option N, don't normalize contents
C     - option D, debugging printout
C     - option O, overflow option
C     - option U, underflow option
C     - option S, statistical comparison
C     - option C, compatibility test
C     - option A, absolute test
C     - option Z, skip ref bin = 0
C     - option R, X-Axis overflow
C     - option L, X-Axis underflow
C     - option T, Y-Axis overflow
C     - option B, Y-Axis underflow
      PARAMETER( NORMD = 1 , DEBUG = 2 , OFLOW = 3, UFLOW = 4,
     +  SOPTN = 5, COPTN = 6 , AOPTN = 7 , ZEROS = 8, XOVER = 9,
     +  XUNDR = 10, YOVER = 11, YUNDR = 12)
C
C----------------------------------------------------------------------
C    Global HDIFFB declarations
C----------------------------------------------------------------------
C
      INTEGER DUMPDV,IDR,IDD,BEGINI,BEGINJ,ENDI,ENDJ
      INTEGER OPTS(NPARMS),XSIZ
      REAL    LAMBDA,LNBIGP,ACDIGT
      LOGICAL TWODIM,WEIGHR,WEIGHD,PROFIL,PSDMR,PSDMD
 
      COMMON/HCDIFB/TWODIM,WEIGHR,WEIGHD,PROFIL,PSDMR,PSDMD,DUMPDV,
     +  IDR,IDD,BEGINI,BEGINJ,ENDI,ENDJ,LAMBDA,OPTS,XSIZ,LNBIGP,ACDIGT
 
 
 

#endif
