*
* $Id: tsbvsl.F,v 1.3 2006/09/15 09:34:52 mclareni Exp $
*
* $Log: tsbvsl.F,v $
* Revision 1.3  2006/09/15 09:34:52  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.2  1997/04/07 10:00:45  mclareni
* Mods for winnt
*
* Revision 1.1.1.1.2.1  1997/01/21 11:25:48  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/03/21 17:19:54  mclareni
* Bvsl
*
*
*CMZ :          06/11/90  10.43.17  by  Michel Roethlisberger/IBM
*-- Author :
* Last modif.: 6 Nov 1990 at 10:30:03
*CMZ :          04/05/90  16.17.27  by  Michel Roethlisberger/IBM
*-- Author :
      PROGRAM TSBVSL
      PARAMETER (LENG=500)
      PARAMETER (LENGB=LENG)
*     PARAMETER (LENGB=(LENG-1)/32 +1)
      PARAMETER (TSVAL=0.5,ISVAL=5,IBOUND=10)
      PARAMETER (S= 2.0 , T = 3.0)
      DIMENSION X(LENG),Y(LENG),Z(LENG),W(LENG),VEC(LENG),IVEC(LENG),
     +         X1(LENG),Y1(LENG),Z1(LENG),W1(LENG),
     +         IX(LENG),IX1(LENG),IY(LENG),IY1(LENG),IZ(LENG),IZ1(LENG),
     +         XB(LENGB),YB(LENGB),ZB(LENG),VECB(LENGB)
      DIMENSION NP(3)
      DATA NP/50,150,300/
*
       WRITE(6,*)'1************************************* '
       WRITE(6,*)' TEST PART FOR VEC. TO SCAL COMP. ROUT.'
       WRITE(6,*)' ************************************* '
*   TEST FOR YLOSB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOSB (EQ)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
#if defined(CERNLIB_GFORTRAN)
        do kkk=1, leng
           iy (kkk) = 0
           iy1(kkk) = 0
        end do
#endif
        CALL WHENEQ(ILENG,VEC,1,TSVAL,IY,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,YB,IFOUND,'EQ')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 100
        ENDIF
      ENDDO
        GO TO 200
 100     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOSB (EQ) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 200  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOSB (EQ) OK '
 
*   TEST FOR YLOSB (NE) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOSB (NE)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENNE(ILENG,VEC,1,TSVAL,IY,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,YB,IFOUND,'NE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 101
        ENDIF
      ENDDO
        GO TO 201
 101     IERR=1
          WRITE(6,*)' ERROR ON SUB. YNESB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 201  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOSB (NE)  OK '
 
*   TEST FOR YLOSB (GT) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOSB (GT) '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT(ILENG,VEC,1,TSVAL,IY,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,YB,IFOUND,'GT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 102
        ENDIF
      ENDDO
        GO TO 202
 102     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOSB (EQ) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 202  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOSB (GT)  OK '
 
*   TEST FOR YLOSB (LT) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOSB (LT) '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFLT(ILENG,VEC,1,TSVAL,IY,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,YB,IFOUND,'LT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 103
        ENDIF
      ENDDO
        GO TO 203
 103     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOSB (LT) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 203  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOSB (LT)  OK '
 
*   TEST FOR YLOSB (GE) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOSB (GE) '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGE(ILENG,VEC,1,TSVAL,IY,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,YB,IFOUND,'GE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 104
        ENDIF
      ENDDO
        GO TO 204
 104     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOSB (GE) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 204  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOSB (GE)  OK '
*   TEST FOR YLOSB (LE) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOSB (LE) '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFLE(ILENG,VEC,1,TSVAL,IY,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,YB,IFOUND,'LE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 105
        ENDIF
      ENDDO
        GO TO 205
 105     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOSB (LE) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 205  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOSB (LE)  OK '
*   TEST FOR IYLOSB (GT) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST IYLOSB (GT)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
          DO L=1,ILENG
           IVEC(L)=INT(VEC(L)*IBAUND)
          ENDDO
*
        CALL WHENIGT(ILENG,IVEC,1,ISVAL,IY,NFOUND)
        CALL IYLOSB(ILENG,IVEC,ISVAL,YB,IFOUND,'GT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 106
        ENDIF
      ENDDO
        GO TO 206
 106     IERR=1
          WRITE(6,*)' ERROR ON SUB. IYLOSB (GT) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 206  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE IYLOSB (GT) OK '
*   TEST FOR IYLOSB (LT) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST IYLOSB (LT) '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
          DO L=1,ILENG
           IVEC(L)=INT(VEC(L)*IBAUND)
          ENDDO
*
        CALL WHENILT(ILENG,IVEC,1,ISVAL,IY,NFOUND)
        CALL IYLOSB(ILENG,IVEC,ISVAL,YB,IFOUND,'LT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 107
        ENDIF
      ENDDO
        GO TO 207
 107     IERR=1
          WRITE(6,*)' ERROR ON SUB. IYLOSB (LT) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 207  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE IYLOSB (LT)  OK '
*   TEST FOR IYLOSB (GE) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST IYLOSB '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
          DO L=1,ILENG
           IVEC(L)=INT(VEC(L)*IBAUND)
          ENDDO
*
        CALL WHENIGE(ILENG,IVEC,1,ISVAL,IY,NFOUND)
        CALL IYLOSB(ILENG,IVEC,ISVAL,YB,IFOUND,'GE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 108
        ENDIF
      ENDDO
        GO TO 208
 108     IERR=1
          WRITE(6,*)' ERROR ON SUB. IYLOSB (GE) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 208  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE IYLOSB  (GE) OK '
*   TEST FOR IYLOSB (LE) SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST IYLOSB (LE)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
          DO L=1,ILENG
           IVEC(L)=INT(VEC(L)*IBAUND)
          ENDDO
*
        CALL WHENILE(ILENG,IVEC,1,ISVAL,IY,NFOUND)
        CALL IYLOSB(ILENG,IVEC,ISVAL,YB,IFOUND,'LE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 109
        ENDIF
      ENDDO
        GO TO 209
 109     IERR=1
          WRITE(6,*)' ERROR ON SUB. IYLOSB FOR (LE) LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 209  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE IYLOSB (LE) OK '
*
       WRITE(6,*)'1************************************* '
       WRITE(6,*)' TEST PART FOR VEC. TO VEC. COMPARIS. '
       WRITE(6,*)' ************************************* '
 
*   TEST FOR YLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOXB (EQ)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
*
        CALL YEQX(ILENG,VEC,X,IY,NFOUND)
        CALL YLOXB(ILENG,VEC,X,YB,IFOUND,'EQ')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 110
        ENDIF
      ENDDO
        GO TO 210
 110     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOXB (EQ) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 210  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOXB (EQ) OK '
 
*   TEST FOR YLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOXB (NE)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
*
        CALL YNEX(ILENG,VEC,X,IY,NFOUND)
        CALL YLOXB(ILENG,VEC,X,YB,IFOUND,'NE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 111
        ENDIF
      ENDDO
        GO TO 211
 111     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOXB (NE) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 211  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOXB (NE) OK '
 
*   TEST FOR YLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOXB (GT)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
*
        CALL YGTX(ILENG,VEC,X,IY,NFOUND)
        CALL YLOXB(ILENG,VEC,X,YB,IFOUND,'GT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 112
        ENDIF
      ENDDO
        GO TO 212
 112     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOXB (GT) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 212  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOXB (GT) OK '
 
*   TEST FOR YLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOXB (LT)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
*
        CALL YLTX(ILENG,VEC,X,IY,NFOUND)
        CALL YLOXB(ILENG,VEC,X,YB,IFOUND,'LT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 113
        ENDIF
      ENDDO
        GO TO 213
 113     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOXB (LT) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 213  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOXB (LT) OK '
 
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOXB (GE)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
*
        CALL YGEX(ILENG,VEC,X,IY,NFOUND)
        CALL YLOXB(ILENG,VEC,X,YB,IFOUND,'GE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 114
        ENDIF
      ENDDO
        GO TO 214
 114     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOXB (GE) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 214  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOXB (GE) OK '
 
*   TEST FOR YLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST YLOXB (LT)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
*
        CALL YLEX(ILENG,VEC,X,IY,NFOUND)
        CALL YLOXB(ILENG,VEC,X,YB,IFOUND,'LT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 115
        ENDIF
      ENDDO
        GO TO 215
 115     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOXB (LT) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 215  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOXB (LT) OK '
 
*   TEST FOR IYLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST IYLOXB (GT)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
        DO L=1,LENG
        IVEC(L)=INT(VEC(L)*IBOUND)
        IX(L)=INT(X(L)*IBOUND)
        ENDDO
*
        CALL IYGTX(ILENG,IVEC,IX,IY,NFOUND)
        CALL IYLOXB(ILENG,IVEC,IX,YB,IFOUND,'GT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 116
        ENDIF
      ENDDO
        GO TO 216
 116     IERR=1
          WRITE(6,*)' ERROR ON SUB. IYLOXB (GT) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 216  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE IYLOXB  OK '
 
*   TEST FOR IYLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST IYLOXB (LT)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
        DO L=1,LENG
        IVEC(L)=INT(VEC(L)*IBOUND)
        IX(L)=INT(X(L)*IBOUND)
        ENDDO
*
        CALL IYLTX(ILENG,IVEC,IX,IY,NFOUND)
        CALL IYLOXB(ILENG,IVEC,IX,YB,IFOUND,'LT')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 117
        ENDIF
      ENDDO
        GO TO 217
 117     IERR=1
          WRITE(6,*)' ERROR ON SUB. IYLOXB (LT) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 217  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE IYLOXB (LT) OK '
 
*   TEST FOR IYLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST IYLOXB '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
        DO L=1,LENG
        IVEC(L)=INT(VEC(L)*IBOUND)
        IX(L)=INT(X(L)*IBOUND)
        ENDDO
*
        CALL IYGEX(ILENG,IVEC,IX,IY,NFOUND)
        CALL IYLOXB(ILENG,IVEC,IX,YB,IFOUND,'GE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 118
        ENDIF
      ENDDO
        GO TO 218
 118     IERR=1
          WRITE(6,*)' ERROR ON SUB. YLOXB (GE) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 218  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE YLOXB (GE) OK '
 
*   TEST FOR IYLOXB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST IYLOXB (LE)'
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
        DO L=1,LENG
        IVEC(L)=INT(VEC(L)*IBOUND)
        IX(L)=INT(X(L)*IBOUND)
        ENDDO
*
        CALL IYLEX(ILENG,IVEC,IX,IY,NFOUND)
        CALL IYLOXB(ILENG,IVEC,IX,YB,IFOUND,'LE')
      CALL BINVEC(ILENG,YB,IY1)
 
*
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 119
        ENDIF
      ENDDO
        GO TO 219
 119     IERR=1
          WRITE(6,*)' ERROR ON SUB. IYLOXB (LE) FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 219  CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE IYLOXB (LE) OK '
 
       WRITE(6,*)'1************************************* '
       WRITE(6,*)' TEST PART FOR MISCELLANEOUS ROUTINES '
       WRITE(6,*)' ************************************* '
*   TEST FOR INTGB SUBROUTINE
 
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST INTGB '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
        CALL NRAN(X,ILENG)
        CALL NRAN(X1,ILENG)
*
 
*
          IERR=0
            DO  I=1,ILENG
             X1(I)=X(I)+1.0
             VEC(I)=VEC(I)*2.
             IY(I)=0.0
             IY1(I)=0.0
            ENDDO
            DO I=1,LENGB
              YB(I)=0
            ENDDO
      CALL INTG(ILENG,VEC,X1,X,IY,NFOUND)
      CALL INTGB(ILENG,VEC,X1,X,YB,IFOUND)
      CALL BINVEC(ILENG,YB,IY1)
*
      DO I=1,ILENG
        IF(IY(I).NE.IY1(I))THEN
        IELEM=I
        GO TO 11
        ENDIF
      ENDDO
        GO TO 21
 11       IERR=1
          WRITE(6,*)' ERROR ON SUB. INTGB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 21   CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE INTGB OK '
 
*   TEST FOR RJCTB SUBROUTINE
 
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST RJCTB '
      DO I1=1,3
      IERR=0
      ILENG=NP(I1)
        DO L=1,ILENG
          Z(L)=L
          IX(L)=L
          W(L)=0.0
          W1(L)=0.0
        ENDDO
        CALL NRANIN(54321.)
        CALL NRAN(Y,ILENG)
        CALL NRAN(Y1,ILENG)
*
        ISW=1
        CALL RJCTB(Y,Z,Y1,W,YB,ILENG,N2,ISW)
        CALL BINVEC(ILENG,YB,IY)
*
        M=0
        DO L=1,ILENG
        IF(Y1(L).GT.Y(L))THEN
           W1(IX(L))=Z(L)
        ELSE
           M=M+1
           IZ(M)=IX(L)
        ENDIF
        ENDDO
        N3=M
        DO L=1,ILENG
        IF(W(L).NE.W1(L))THEN
        IERR=1
        WRITE(6,*)' ERROR FOR OUT' ,L
        ENDIF
        ENDDO
      ENDDO
        IF(ISW.EQ.1)IERR=2
        IF(IERR.EQ.0)WRITE(6,*)' TEST RJCTB OK'
        IF(IERR.EQ.2)WRITE(6,*)' ERROR FOR ISW'
       WRITE(6,*)'1************************************* '
       WRITE(6,*)' TEST PART FOR EXTENDED L. A. ROUTINES '
       WRITE(6,*)' ************************************* '
*   TEST FOR DOTB FUNCTION
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST DOTB '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT(ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,VECB,IFOUND,'GT')
 
*
            DO I=1,ILENG
             X(I)=I
             Y(I)=I
            ENDDO
        BDOT=DOTB(ILENG,VECB,X,Y)
        SDOT=DOT(NFOUND,IVEC,X,Y)
        IF(SDOT.NE.BDOT)THEN
        IERR=1
          WRITE(6,*)' ERROR ON FUNCTION DOTB FOR LENGTH = ', ILENG
        ENDIF
      ENDDO
        IF(IERR.NE.1)WRITE(6,*)' FUNCTION DOTB  OK '
          WRITE(6,*)' #####################################'
*   TEST FOR COPYB SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST COPYB '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT(ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,VECB,IFOUND,'GT')
 
*
            DO  I=1,ILENG
             X(I)=I
             Y(I)=0.0
             Y1(I)=0.0
            ENDDO
      CALL COPY(NFOUND,IVEC,Y,X)
      CALL COPYB(ILENG,VECB,Y1,X)
*
      DO I=1,ILENG
        IF(Y(I).NE.Y1(I))THEN
        IELEM=I
        GO TO 10
        ENDIF
      ENDDO
        GO TO 20
 10      IERR=1
          WRITE(6,*)' ERROR ON SUB. COPYB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 20   CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE COPYB  OK '
*   TEST FOR SXYB SUBROUTINE
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST SXYB '
      IERR=0
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT(ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,VECB,IFOUND,'GT')
 
*
            DO  I=1,ILENG
             X(I)=I
             Y(I)=2.*I
             Y1(I)=Y(I)
            ENDDO
      CALL SXY(NFOUND,IVEC,Y,X,S)
      CALL SXYB(ILENG,VECB,Y1,X,S)
*
      DO I=1,ILENG
        IF(Y(I).NE.Y1(I))THEN
        IELEM=I
        GO TO 12
        ENDIF
      ENDDO
        GO TO 22
 12   IERR=1
          WRITE(6,*)' ERROR ON SUB. SXYB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 22   CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE SXYB  OK '
*   TEST FOR SXPYB SUBROUTINE
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST SXPYB '
      IERR=0
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT(ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,VECB,IFOUND,'GT')
 
*
            DO  I=1,ILENG
             X(I)=I
             Y(I)=2.*I
             Y1(I)=Y(I)
            ENDDO
      CALL SXPY(NFOUND,IVEC,Y,X,S)
      CALL SXPYB(ILENG,VECB,Y1,X,S)
*
      DO I=1,ILENG
        IF(Y(I).NE.Y1(I))THEN
        IELEM=I
        GO TO 13
        ENDIF
      ENDDO
        GO TO 23
 13     IERR=1
          WRITE(6,*)' ERROR ON SUB. SXPYB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 23   CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE SXPYB  OK '
*   TEST FOR VXPYB SUBROUTINE
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST VXPYB '
      IERR=0
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT(ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,VECB,IFOUND,'GT')
 
*
            DO  I=1,ILENG
             X(I)=I
             W(I)=0.5*I
             Y(I)=2.*I
             Y1(I)=Y(I)
            ENDDO
      CALL VXPY(NFOUND,IVEC,Y,X,W)
      CALL VXPYB(ILENG,VECB,Y1,X,W)
*
      DO I=1,ILENG
        IF(Y(I).NE.Y1(I))THEN
        IELEM=I
        GO TO 14
        ENDIF
      ENDDO
        GO TO 24
 14    IERR=1
          WRITE(6,*)' ERROR ON SUB. VXPYB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 24   CONTINUE
      ENDDO
          WRITE(6,*)' SUBROUTINE VXPYB  OK '
*   TEST FOR XYPWZB SUBROUTINE
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST XYPWZB '
        IERR=0
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT(ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,VECB,IFOUND,'GT')
 
*
            DO  I=1,ILENG
             X(I)=I
             Y(I)=2.*I
             Y1(I)=Y(I)
             W(I)=I
             Z(I)=0.5*I
            ENDDO
      CALL XYPWZ(NFOUND,IVEC,S,Y,X,T,W,Z)
      CALL XYPWZB(ILENG,VECB,S,Y1,X,T,W,Z)
*
      DO I=1,ILENG
        IF(Y(I).NE.Y1(I))THEN
        IELEM=I
        GO TO 15
        ENDIF
      ENDDO
        GO TO 25
 15     IERR=1
          WRITE(6,*)' ERROR ON SUB. XYPWZB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 25   CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE XYPWZB  OK '
*   TEST FOR SCALB SUBROUTINE
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST SCALB '
      IERR=0
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT(ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB(ILENG,VEC,TSVAL,VECB,IFOUND,'GT')
 
*
            DO  I=1,ILENG
             Y(I)=2.*I
             Y1(I)=Y(I)
            ENDDO
      CALL SCAL(NFOUND,IVEC,Y,S)
      CALL SCALB(ILENG,VECB,Y1,S)
*
      DO I=1,ILENG
        IF(Y(I).NE.Y1(I))THEN
        IELEM=I
        GO TO 17
        ENDIF
      ENDDO
        GO TO 27
 17   IERR=1
          WRITE(6,*)' ERROR ON SUB. SCALB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 27   CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE SCALB  OK '
*   TEST FOR VSETB SUBROUTINE
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST VSETB '
      IERR=0
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT (ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB   (ILENG,VEC,  TSVAL,VECB,IFOUND,'GT')
 
*
            DO  I=1,ILENG
             Y(I)=2.*I
             Y1(I)=Y(I)
            ENDDO
      CALL VSET(NFOUND,IVEC,Y,S)
      CALL VSETB(ILENG,VECB,Y1,S)
*
      DO I=1,ILENG
        IF(Y(I).NE.Y1(I))THEN
        IELEM=I
        GO TO 16
        ENDIF
      ENDDO
        GO TO 26
 16   IERR=1
          WRITE(6,*)' ERROR ON SUB. VSETB FOR LENGTH = ', ILENG
          WRITE(6,*)' AND ELEMENT = ' , IELEM
 26   CONTINUE
      ENDDO
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE VSETB  OK '
*
       WRITE(6,*)'1************************************* '
       WRITE(6,*)' TEST FOR NON LINEAR OPERATIONS       .'
       WRITE(6,*)' ************************************* '
*........................................................
*   TEST FOR SMOD3B SUBROUTINE
      IERR=0
          WRITE(6,*)' #####################################'
          WRITE(6,*)' TEST SMOD3B '
      DO J=1,3
        ILENG=NP(J)
        CALL NRANIN(54321.)
        CALL NRAN(VEC,ILENG)
*
        CALL WHENFGT (ILENG,VEC,1,TSVAL,IVEC,NFOUND)
        CALL YLOSB   (ILENG,VEC,  TSVAL,VECB,IFOUND,'GT')
 
*
            DO I=1,ILENG
             X(I)=I
             Y(I)=2*I
             Z(I)=3*I
             W(I)=-999.
             X1(I)=I
             Y1(I)=2*I
             Z1(I)=3*I
             W1(I)=-999.
            ENDDO
 
        CALL SMOD3B (ILENG,X,Y,Z,W,VECB)
        CALL SMOD3  (NFOUND,X1,Y1,Z1,W1,IVEC)
 
        DO I = 1,ILENG
          IF (W(I).NE.W1(I)) THEN
            IERR = 1
            WRITE(6,*)' ERROR ON SUBROUTINE SMOD3B FOR LENGTH = ', ILENG
            WRITE(6,*)'                     ELEMENT           = ', I
            GOTO 401
          ENDIF
        ENDDO
  401   CONTINUE
      ENDDO
 
      IF(IERR.NE.1)WRITE(6,*)' SUBROUTINE SMOD3B  OK '
      WRITE(6,*)' #####################################'
      STOP
      END
