*
* $Id: sifam3.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sifam3.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIFAM3
C
C    ......................................................
C
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      DIMENSION DIM(10)
C
C     FAMILY OF 300   PLOT
C
C
C---   ARRAY MODIS(15)
C
C---  MODIS(15)     XTICKS  (0,1 OR 2)
C---  MODIS(14)     YTICKS  (0,1 OR 2)
C---  MODIS(13)     XLABELS (0,1 OR 2)
C---  MODIS(12)     YLABELS (0,1 OR 2)
C---  MODIS(11)
C---  MODIS(10)-    XDIVS   (3* 1-15)
C---  MODIS( 9)
C---  MODIS( 8)
C---  MODIS( 7) -   YDIVS   (3* 1-15)
C---  MODIS( 6)
C
C--- MODIS( 5)      LOGY
C--- MODIS( 4)      LOGX
C--- MODIS( 3)      FRAME
C--- MODIS( 2)      GRID
C--- MODIS(1)       AXES
C
C
C
C
C
      IF(KLASS.EQ.100)GO TO 10001
C
C   IS KLASS VALUE WITHIN THE BOUNDS OF COMPUTED GOTO BELOW
      IF(KLASS.LT.1 .OR.KLASS.GT.33)GOTO 996
      IF(KLASS.LT.18.OR.(KLASS.GT.28.AND.KLASS.NE.31))GOTO 1425
C   IF NOT ONE ARG
      CALL SINEXT(ITE)
      IF(ITE.NE.1) GOTO 9916

C   MISSING INDEX MEANINGLESS IN THIS OPERATOR
      CALL SISTAK(0,MP,MN)
      IF(MN.EQ.MISIDX) GOTO 9918
      DIM(1)=1.0
      CALL SINGET(ISI,0,DIM)
      IF(IERRNO.NE.0)RETURN
      IF(ISI.GE.3) GO TO 1426
      X=DYNA(IADDR)
 1425 CONTINUE
C
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,317,16,321,322,323,324,325
     ,,326,327,327,327,327,333,333,335,336,337
     ,,340,340)KLASS
C
C
 1    CONTINUE
C
C
C 301                           PLOTBEGIN
C
C
C******** 301     ************
C
      GO TO 16
C
C
C     *****     BITS OF MAINKT    ****
C  0  SITRAC
C  1  PRINT
C  4  PLOT
C  5  ARGUS
C
 2    CONTINUE
C
C
C 302                           !TRACE
C
C
      MAINKT(1)=1
C
C******** 302     ************
C
      GO TO 16
C
 3    CONTINUE
C
C
C303                           !NOTRACE
C
C******** 303     ************
C
C
      MAINKT(1)=0
C
      GO TO 16
C
 4    CONTINUE
C
C
C304                           !PRINT
C
C******** 304     ************
C
      MAINKT(2)=11
C
C
      GO TO 16
C
 5    CONTINUE
C
C
C305                           !NOPRINT
C
C******** 305     ************
C
      MAINKT(2)=0
C
C
      GO TO 16
 6    CONTINUE
C
C
C306                           !LABELS
C
C******** 306     ************
C
      GO TO  24
C
C
 7    CONTINUE
C
C
C307                           /BYE
C
C
C
      GO TO  24
C
C307
C******** 307     ************
C
 8    CONTINUE
C308
C
C308                           !WAIT
C******** 308     ************
C
        CALL SIWAIT
C
C
      GO TO 16
C
 9    CONTINUE
C
C
C309                           !CLEAR
C
C309
C******** 309     ************
C
C
      GO TO  24
C
 10   CONTINUE
C
C
C310                           !NAMES
C******** 310     ************
      CALL KXLISV
      GO TO 16
C
 11   CONTINUE
C
C
C311                           !PUNCH
C******** 311     ************
      GO TO  24
C
 12   CONTINUE
C
C
C 312                         !STOP
C******** 312     ************
C
C
      KLASS=3
      GOTO 16
C
 13   CONTINUE
C
C
C
C
C313                           !STATUS
C******** 313     ************
C
      PRINT *,'  STATUS OF SYSTEM VARIABLES'
      PRINT *,' !LENGTH IS: ',I8LEN
      PRINT *,' !DIGITS IS: ',I8DGT
      PRINT *,' !TRACE  IS: ',MAINKT(1)
      PRINT *,' !PRINT  IS: ',MAINKT(2)
      GO TO 999
C
 14   CONTINUE
C
C
C314                           !HELP
C******** 314     ************
C
C
      GO TO  24
C
C          !NEWS
C
 317  CONTINUE
C
       PRINT 123
123    FORMAT(
     C/,'    Features not yet implemented'
     C/,  ' ',
     C/,'    1.      Indexing (LHS). '
     C/,  ' ',
     C/,'    Known bugs - VERSION 1 August 1993  '
     C/,  ' ',
     C/,'    This is the list of presently known bugs: '
     C/,  ' ',
     C/,'    1. None')
C
      GO TO 16
C
 322  CONTINUE
      GO TO 17
C                             !LENGTH <EXPR>
 323  I8LEN=X+.5
      IF((I8LEN.GE.1).AND.(I8LEN.LE.130))GO TO 17
C     I8LEN IS OUT OF RANGE
      IF(I8LEN.LT.1)GOTO 423
C
      I8LEN=130
      WRITE(NPRINT,3003)
3003  FORMAT(' LINE LENGTH RESET TO 130')
      GO TO 17
C
423   CONTINUE
      I8LEN=1
      WRITE(NPRINT,3004)
3004  FORMAT(' LINE LENGTH RESET TO 1')
      GO TO 17
C
C
 324  I8DGT=X+.5
      IF((I8DGT.GE.1).AND.(I8DGT.LE.14))GO TO 17
         PRINT 3951
3951       FORMAT( ' DIGITS......')
      IF(I8DGT.LT.1) GOTO 424
C
      I8DGT=14
      WRITE(NPRINT,3001)
3001  FORMAT(' NUMBER OF DIGITS SET TO LARGEST AVAIL. NUMBER=14')
      GO TO 17
C
424   I8DGT=1
      WRITE(NPRINT,3002)
3002  FORMAT(' NUMBER OF DIGITS FOR OUTPUT SET TO 1')
      GO TO 17
C
 325  CONTINUE
      GO TO 17
 326  CONTINUE
      GO TO 17
C XTICKS,YTICKS,XLABELS,YLABELS
327    CONTINUE
*      I=104-KLASS-KLASS
*       J=I+1
*      K=0
       IF(X.LT.0.)GOTO 32703
       IF(X.EQ.0.)GOTO 32702
*       K=K+1
32702 CONTINUE
*     K=K+1
32703 CONTINUE
*     MODIS(J)=K
      GO TO 17
C XDIVS,YDIVS
333   I=13-3*(KLASS-26)
      L=I-1
      M=MODE+MODE
      GO TO 33308
33301 I=I-1
*      X=DYNA(IADDR+MODE)
      IF(LENGTH.LT.M)GOTO 17
      GOTO 33308
33302 I=I-1
*      X=DYNA(IADDR+M)
      IF(LENGTH.LE.M)GO TO 17
33308 CONTINUE
*     J=I+1
*      K=X+.5
*      IF(K.GT.0.AND.K.LT.16)GO TO 33311
*      WRITE(NPRINT,33310)(L-I)/4+2
*33310 FORMAT(' LEVEL',I2,' DIVISIONS <1 OR >15 - NOT CHANGED.')
*      GO TO 33312
*33311 CONTINUE
*     MODIS(J)=K
33312 IF(I-L)17,33302,33301
C
 1426  CALL SINERR(2)
       RETURN
C
C    321        DUMP
 321  CONTINUE
      GO TO  24
C
10001 CONTINUE
      KLASS=4
C
      GO TO 16
335   CONTINUE
      GOTO16
C********SMALL
336   CONTINUE
C********LARGE
      GOTO16
 337  CONTINUE
C
C 337                         !PAUSE <EXPR>
      GO TO  24
340   CONTINUE
      PRINT 3332

3332  FORMAT(' **********************************************'
     C,/,    ' *        SIGMA Version 1 August 1993         *'
     C,/,    ' * Type:  EXIT  to leave SIGMA                *'
     C,/,    ' *        @ to show the last 10 commands.     *'
     C,/,    ' **********************************************')

      GO TO 999
 24   CALL SINERR (41)
      GO TO 999
C
17    CONTINUE
      CALL SIUSTK
16    CONTINUE
      GO TO 999
996   CALL SINERR(6)
      GOTO 999
9916  CALL SINERR(16)
      GOTO 999
9918  CALL SINERR(18)

 999  END
