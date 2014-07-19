*
* $Id: texarr.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: texarr.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      SUBROUTINE TEXARR(LINE,JPAK,IFTX,LEVL,MXAR,NACT,RARR)
*     -----------------------****-****-----------****-====-
*
      CHARACTER*(*) LINE
      REAL*4    RARR(MXAR)
      INTEGER*4 IARR(MXAR)
*
      LOGICAL IFREAL
      CHARACTER*4 PRFX
*
      IFREAL=.true.
      GOTO 10
*
      ENTRY      TEXARI(LINE,JPAK,IFTX,LEVL,MXAR,NACT,IARR)
*     -----------------------****-****-----------****-====-
*
      IFREAL=.false.
*
   10 CONTINUE
*
      IF(NACT.LT.0.OR.NACT.GT.MXAR) NACT=0
*
      NAC=0
      IOK=0
      RDV=0
      IDV=0
   12      CONTINUE
      CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
      NXL=IFTX-(IFTX/4)*4
*
      IF(IOK.LE.0) THEN
        IF(PRFX.EQ.'=') THEN
          NAC=MAX0(NAC,NACT)
          IOK=1
*
        ELSE IF(PRFX.EQ.'$') THEN
          IDV=NUMB
          IF(IDV.EQ.0) IDV=1
          RDV=FNUM
          IF(RDV.EQ.0) RDV=1
         ELSE IF(PRFX.EQ.'*') THEN
          NAC=MAX0(NAC,1)
          IF(IFREAL) THEN
            RV=RARR(NAC)
          ELSE
            IV=IARR(NAC)
          ENDIF
*
          DO I=NAC+1,MIN0(NAC+NUMB-1,MXAR)
            IF(IFREAL) THEN
              RV=RV+RDV
              RARR(I)=RV
            ELSE
              IV=IV+IDV
              IARR(I)=IV
            ENDIF
          ENDDO
          NAC=MIN0(NAC+NUMB-1,MXAR)
*
        ELSE IF(PRFX.EQ.'>') THEN
          IF(IDV.EQ.0) IDV=1
          IF(RDV.EQ.0) RDV=1.
          NAC=MAX0(NAC,1)
 
          IF(IFREAL) THEN
            RV=RARR(NAC)
            NN=ABS(FNUM-RV)/ABS(RDV)+1.5
            IF(FNUM.LT.RV) RDV=-ABS(RDV)
          ELSE
            IV=IARR(NAC)
            NN=IABS(NUMB-IV)/IABS(IDV)+1
            IF(NUMB.LT.IV) IDV=-IABS(IDV)
          ENDIF
*
          DO I=NAC+1,MIN0(NAC+NN-1,MXAR)
            IF(IFREAL) THEN
            RV=RV+RDV
            RARR(I)=RV
            ELSE
            IV=IV+IDV
            IARR(I)=IV
            ENDIF
          ENDDO
          NAC=MIN0(NAC+NN-1,MXAR)
*
        ELSE
          IF(IFTX/8.GT.0) THEN
            NAC=NAC+1
            IF(     IFREAL) RARR(NAC)=FNUM
            IF(.NOT.IFREAL) IARR(NAC)=NUMB
          ELSE IF(NXL.GE.LEVL) THEN
            NAC=NAC+1
          ENDIF
        ENDIF
*
        IF(NAC.GE.MXAR) THEN
          NACT=MXAR
          IOK=1
        ENDIF
      ENDIF
*
      IF(NXL.GE.LEVL) GOTO 12
*
      IF(IOK.EQ.0) NACT=NAC
      END
