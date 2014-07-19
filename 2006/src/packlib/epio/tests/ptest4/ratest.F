*
* $Id: ratest.F,v 1.1.1.1 1996/03/08 15:21:46 mclareni Exp $
*
* $Log: ratest.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:46  mclareni
* Epio
*
*
      PROGRAM RATEST
*
*--- Step 3: read in random access
*
      DIMENSION IHEAD(4),IW(31),IOUT(3000),IBUF(5000)
      DIMENSION IB(100),IP(100)
      DATA LUN/11/,MODE/3/,MULT/100/,MB/0/
 
      PRINT 10000
10000 FORMAT(//' ++++++++++++ EPIO random access test',
     +         ' ++++++++++++'//
     +         '              pass 3 starting execution'/)
 
      CALL EPINIT
      NR=0
#if defined(CERNLIB_IBMMVS)
      OPEN(LUN,FILE='gg.uuu.TESTDFIX',
     +STATUS='OLD',ACCESS='DIRECT',RECL=3600)
#endif
#if defined(CERNLIB_IBM)
C     CALL VMCMS('FI RNDFILE DISK RANTEST DATA A (RECFM F '//
C    +'BLKSIZE 3600 XTENT 99999',IVMERR)
      OPEN(LUN,FILE='/RANTEST DATA A',STATUS='OLD',ACCESS='DIRECT',
     +RECL=3600)
#endif
#if defined(CERNLIB_CRAY)
      OPEN(LUN,FILE='rftest.data',STATUS='OLD',ACCESS='DIRECT',
     +RECL=3600,IOSTAT=IOS)
#endif
#if defined(CERNLIB_VAX)
      OPEN(LUN,FILE='TEST.DATA',STATUS='OLD',ACCESS='DIRECT',
     +RECL=900)
#endif
*--- read block numbers and off-sets from file created in step 2
      READ(22,*)  MB,(IB(I),IP(I),I=1,MB)
*--- first read (in inverse order) five records (except from the last
*    block) starting from the record the off-set of which has been kept
*    (I.e. to read one record one needs first a call to EPDACR and then
*     a call to EPREAD, as below).
      DO 10  I=MB,1,-1
      CALL EPDACR(LUN,IB(I),IP(I),IERR)
      IF(IERR.NE.0)  THEN
         PRINT *,' EPDACR error'
         GOTO 100
      ENDIF
      IF(I.EQ.MB)  THEN
         NUP=1
      ELSE
         NUP=5
      ENDIF
      DO 20  J=1,NUP
      CALL EPREAD(LUN,MODE,NW,IOUT,IBUF,IERR)
      IF(IERR.NE.0)  GOTO 100
      NR=5*(I-1)+J
*
*--- the code following below only checks the record contents
*
      PRINT *,' NR, NW, 5 words: ',NR,NW,(IOUT(K),K=1,5)
      IF(NW.NE.NR*MULT)  THEN
         PRINT *,' wrong length: ',NW,' at record: ',NR
         GOTO 100
      ENDIF
      DO 30  K=1,NW
         IF(IOUT(K).NE.NR)  THEN
            PRINT *,' wrong contents: ',IOUT(K),' record: ',NR,
     +      ' word: ',K
            GOTO 100
         ENDIF
   30 CONTINUE
   20 CONTINUE
   10 CONTINUE
  100 CONTINUE
      IF(IERR.LE.1)  THEN
      PRINT 10010
10010 FORMAT(//' ------------ EPIO random access test',
     +         ' ------------'//
     +         '              pass 3 terminating - all done'/)
      ELSE
      PRINT *,' ********** STOP because of EPIO error.'
      ENDIF
      END
