*
* $Id: pafort.F,v 1.6 1998/11/10 10:51:25 couet Exp $
*
* $Log: pafort.F,v $
* Revision 1.6  1998/11/10 10:51:25  couet
* - A comma was missing in a FORMAT. It produced a warning on linux.
*
* Revision 1.5  1998/03/19 15:13:59  couet
* - The unit command print the file names on several lines if they are too
*   long
*
* Revision 1.4  1998/02/11 15:48:04  couet
* - KUIP/UNITS has been replaced by FORTRAN/UNITS and is now able to list the
*   HBOOK files again.
*
* Revision 1.3  1996/06/27 15:41:23  couet
* - Dummy command F/UNIT suppressed (ambiguous with K/UNITS)
*
* Revision 1.2  1996/03/20 09:30:58  couet
* The array LAD is initialized with 0
*
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :          10/01/96  18.13.01  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAFORT
*
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
#include "paw/pawlun.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "hbook/hcpiaf.inc"
#include "hbook/hcminpu.inc"
*
      COMMON/CSUNIT/LUNINP,LUNPM,LUNFIL,LUNLOG,LUNMAP,LUNLIB,
     +                     ISTPM,ISTFIL,ISTLOG,ISTMAP,ISTLIB
      CHARACTER*32 CHPATH,CHFORM
      COMMON/PCFORT/CHPATH(10),CHFORM
      CHARACTER*1 TYPE
      DIMENSION ITOK(2,10),IPAR(10),RPAR(10),LAD(10)
      LOGICAL CLALL
#if defined(CERNLIB_COMIS)
      DOUBLE PRECISION CSDJFN
      INTEGER CSIJFN
      COMMON/PCCSFN/DFNVAL,IFNVAL,RFNVAL
      DOUBLE PRECISION DFNVAL
#endif
#if (defined(CERNLIB_COMIS))&&(defined(CERNLIB_VAX))
      INCLUDE '($DSCDEF)'
      RECORD /DSCDEF1/ IDESCR(10)
#endif
*.    ------------------------------------------------------------------
*.
      CALL KUPATL(CHPATL,NPAR)
*.______________________________________
*
*             Application HMINUIT
*
      IF(CHPATL.EQ.'HMINUIT')THEN
         CALL KUAPPL(LUN,MACFLG,CHFORM)
         IF(MACFLG.EQ.0)GO TO 99
         NCMINP=0
   1     CALL KUREAD(LUN,CHINPU(NCMINP+1),NCH)
         IF(NCH.LT.0)GO TO 99
         NCMINP=NCMINP+1
         IF(NCMINP.LT.MCHINP)GO TO 1
         GO TO 99
      ENDIF
*.______________________________________
*
*             /FORTRAN/COMIS
*
      IF(CHPATL.EQ.'COMIS')THEN
#if defined(CERNLIB_COMIS)
         CALL PAWCS
         CALL KUAPPL(LUNINP,MACFLG,CHFORM)
         IF(MACFLG.EQ.0)LUNINP=5
         CALL CSPAUS('PAW')
#endif
#if (defined(CERNLIB_COMIS))&&(defined(CERNLIB_CZ))
         CALL PFCSEX(LUNINP,' ',IER)
#endif
         GO TO 99
      ENDIF
*.______________________________________
*
*             /FORTRAN/UNITS
*
      IF(CHPATL.EQ.'UNITS')THEN
         CALL KXUNIT
         DO 50 I=1,NCHTOP
            IF (ICHTOP(I).NE.0) THEN
               L  = LENOCC(HFNAME(I))
               I1 = 1
               I2 = MIN(L,36)
               WRITE(6,2100)ICHTOP(I),HFNAME(I)(I1:I2),' ','DIRECT',' '
  2            IF (I2.LT.L) THEN
                  I1 = I2+1
                  I2 = MIN(I2+36,L)
                  WRITE(6,2099)HFNAME(I)(I1:I2),' ',' ',' '
                  GOTO 2
               ENDIF
2099           FORMAT
     +      (1X,'|    | ',A,T47,'| ',A,T60,'| ',A,T72,'|',A,T78,' |')
2100           FORMAT
     +      (1X,'|',I3,' | ',A,T47,'| ',A,T60,'| ',A,T72,'|',A,T78,' |')
            ENDIF
   50    CONTINUE
         IF (NCHTOP.GT.1) THEN
            WRITE(6,2200)
2200        FORMAT
     +  (' +',4('-'),'+',39('-'),'+',12('-'),'+',11('-'),'+',6('-'),'+')
         ENDIF
      ENDIF
*.______________________________________
*
*             /FORTRAN/FILE
*
      IF(CHPATL.EQ.'FILE')THEN
         CALL KUGETI(LUN)
         CALL PALUNF(LUN,1,IFREE)
         IF(IFREE.NE.0)GO TO 99
         CALL KUGETF(CHFILE,NCH1)
         CALL KUGETC(CHTEMP,NCH2)
         CALL KUOPEN(LUN,CHFILE,CHTEMP,ISTAT)
         IF(ISTAT.NE.0) THEN
            PRINT *,' *** Cannot open file '//CHFILE(:MAX(1,NCH1))
         ELSE
            LUNIT(LUN)=9
         ENDIF
         GO TO 99
      ENDIF
*.______________________________________
*
*             /FORTRAN/CLOSE
*
      IF(CHPATL.EQ.'CLOSE')THEN
         CALL KUGETI(LUN)
         CLALL = .FALSE.
         IF (LUN .EQ. 0) THEN
            LUN = 1
            CLALL = .TRUE.
         ENDIF
*
   3     IF (CLALL) THEN
            CALL PALUNF(LUN,-2,IFREE)
         ELSE
            CALL PALUNF(LUN,2,IFREE)
         ENDIF
         IF (IFREE.EQ.0 .OR. IFREE.EQ.6 .OR. IFREE.EQ.7) GOTO 98
         IF(LUNIT(LUN).EQ.1)THEN
            CALL FZENDI(LUN,'TX')
         ENDIF
         IF(LUNIT(LUN).EQ.2)THEN
            CALL FZENDO(LUN,'TX')
         ENDIF
         IF(LUNIT(LUN).GT.2.AND.LUNIT(LUN).LT.6)THEN
            DO 5 I=1,NCHTOP
               IF (ICHTOP(I) .GT. 1000) THEN
#if defined(CERNLIB_CZ)
                  IF(ICHLUN(I).EQ.LUN) THEN
*
*-- send close command to piafserv
*
                      WRITE(CHSMPF,'(A,I4)') 'fortran/close', LUN
                      CALL PFKUIP(CHSMPF, ISTAT)
                      IF (ISTAT .NE. 0) THEN
                         CALL HBUG('Cannot close file on Piaf server',
     +                             'PAFORT',0)
                         GOTO 98
                      ENDIF
                      CHTEMP = CHTOP(I)
                      CALL HREND(CHTEMP)
                      GOTO 7
                  ENDIF
#endif
               ELSEIF(ICHTOP(I).EQ.LUN)THEN
                  CHTEMP=CHTOP(I)
                  CALL HRENDC(CHTEMP)
                  GOTO 7
               ENDIF
   5        CONTINUE
         ENDIF
         IF(LUNIT(LUN).EQ.8)THEN
            IF (.NOT.SERVPF) THEN
               CALL IGQWK(2,'OPEN',RVAL)
               IF(RVAL.NE.0.)CALL IGMETA(999,0)
            ENDIF
         ENDIF
         CALL PACLOS(LUN)
   7     LUNIT(LUN)=0
  98     IF (CLALL) THEN
            LUN = LUN + 1
            IF (LUN .LE. 128) GOTO 3
         ENDIF
         GO TO 99
      ENDIF
*.______________________________________
*
*             /FORTRAN/REWIND
*
      IF(CHPATL.EQ.'REWIND')THEN
         CALL KUGETI(LUN)
         CALL PALUNF(LUN,2,IFREE)
         IF(IFREE.EQ.0)GO TO 99
         IF(IFREE.EQ.6.OR.IFREE.EQ.7)GO TO 99
         IF(LUNIT(LUN).EQ.1)THEN
            CALL FZENDI(LUN,'I')
            GO TO 99
         ENDIF
         IF(LUNIT(LUN).EQ.2)THEN
            CALL FZENDO(LUN,'I')
            LUNIT(LUN)=1
            GO TO 99
         ENDIF
         IF(LUNIT(LUN).GT.2.AND.LUNIT(LUN).LT.6)GO TO 99
         REWIND LUN
         GO TO 99
      ENDIF
*.______________________________________
*
*             /FORTRAN/LOOP
*             /FORTRAN/CALL
*
      NTIMES=1
      IF(CHPATL.EQ.'LOOP')THEN
         CALL KUGETI(NTIMES)
         IF(NTIMES.LE.0)GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'CALL'.OR.CHPATL.EQ.'LOOP')THEN
#if defined(CERNLIB_COMIS)
         CALL KUSPY('ON')
         CALL KUGETF(CHFILE,NCHTOT)
         CALL KUSPY('OFF')
         NCHF = NCHTOT
         I = INDEX(CHFILE(1:NCHTOT),'(')
         IF (I .NE. 0) NCHF = I - 1
         IF( CHFILE(1:1).EQ.'=') THEN
            CHTITL = CHFILE(3:NCHF)
            NCHF = NCHF - 2
         ELSE
            CHTITL = CHFILE(1:NCHF)
         ENDIF
*
         DO 22 I=1,10
            LAD(I) = 0
  22     CONTINUE
         CALL PAWCS
         CALL PAWFCA(CHTITL,NCHF,JAD,1)
         IF(JAD.EQ.0)GO TO 99
*
         CALL KUGETE(CHFILE,NCHTOT)
         NCHF = NCHTOT
         I = INDEX(CHFILE(1:NCHTOT),'(')
         IF (I .NE. 0) NCHF = I - 1
         IF(NCHF.EQ.NCHTOT)THEN
            DO 21 ITIMES=1,NTIMES
               CALL CSJCAL(JAD,0,X,X,X,X,X,X,X,X,X,X)
  21        CONTINUE
         ELSE
            CALL KILEXP(CHFILE(NCHF+2:NCHTOT-1),',','''','''',10,
     +                         ITOK,NTOK,' ')
            CHFORM='('
            NCHC=1
            DO 30 I=1,NTOK
               I1=ITOK(1,I)+NCHF+1
               I2=ITOK(2,I)+NCHF+1
               CHTITL=CHFILE(I1:I2)
               CALL KUDPAR(CHTITL,IPAR(I),RPAR(I),CHPATH(I),N,TYPE)
               CHTEMP=CHFORM
               LAD(I)=LOCF(IPAR(I))-LOCF(IQ(1))+1
               IF(TYPE.EQ.'I')THEN
                  CHFORM=CHTEMP(1:NCHC)//'I,'
                  NCHC=NCHC+2
               ELSEIF(TYPE.EQ.'R')THEN
                  CHFORM=CHTEMP(1:NCHC)//'R,'
                  LAD(I)=LOCF(RPAR(I))-LOCF(IQ(1))+1
                  NCHC=NCHC+2
               ELSE
                  IF(CHFILE(I1:I1).EQ.''''.AND.CHFILE(I2:I2)
     +                            .EQ.'''')THEN
                     CHFORM=CHTEMP(1:NCHC)//'*32,'
#endif
#if (defined(CERNLIB_COMIS))&&(!defined(CERNLIB_VAX))
                     LAD(I)=LOCF(CHPATH(I))-LOCF(IQ(1))+1
#endif
#if (defined(CERNLIB_COMIS))&&(defined(CERNLIB_VAX))
*
* For VMS we cannot use LOCF(CHPATH(I)) because it returns the address
* of a temporary descriptor which will be destroyed by subsequent string
* operations.  Instead we have to construct our own description and pass
* its address to COMIS.
*
                     IDESCR(I).DSC$B_DTYPE = DSC$K_DTYPE_T
                     IDESCR(I).DSC$B_CLASS = DSC$K_CLASS_S
                     IDESCR(I).DSC$W_MAXSTRLEN = LEN(CHPATH(I))
                     IDESCR(I).DSC$A_POINTER =  %LOC(CHPATH(I))
                     LAD(I) = LOCF(IDESCR(I)) - LOCF(IQ(1)) + 1
#endif
#if defined(CERNLIB_COMIS)
                     CHTITL=CHPATH(I)
                     CHPATH(I)=CHTITL(2:I2-I1)
                     NCHC=NCHC+4
                  ELSE
                     CALL CLTOU(CHTITL)
                     CALL KUVECT(CHTITL,LX1,LX2)
                     IF(LX1.NE.0)THEN
                        CHFORM=CHTEMP(1:NCHC)//'R,'
                        LAD(I)=LX1
                        NCHC=NCHC+2
                     ENDIF
                  ENDIF
               ENDIF
  30        CONTINUE
*
            CHFORM(NCHC:NCHC)=')'
            DO 40 ITIMES=1,NTIMES
               IF(CHFILE(1:1).EQ.'=') THEN
                  IF(CHFILE(2:2).EQ.'D') THEN
                     DFNVAL = CSDJFN(JAD,CHFORM,
     +                    Q(LAD(1)),Q(LAD(2)),Q(LAD(3)),
     +                    Q(LAD(4)),Q(LAD(5)),Q(LAD(6)),
     +                    Q(LAD(7)),Q(LAD(8)),Q(LAD(9)),Q(LAD(10)))
                  ELSEIF(CHFILE(2:2).EQ.'I') THEN
                     IFNVAL = CSIJFN(JAD,CHFORM,
     +                    Q(LAD(1)),Q(LAD(2)),Q(LAD(3)),
     +                    Q(LAD(4)),Q(LAD(5)),Q(LAD(6)),
     +                    Q(LAD(7)),Q(LAD(8)),Q(LAD(9)),Q(LAD(10)))
                  ELSE
                     RFNVAL = CSRJFN(JAD,CHFORM,
     +                    Q(LAD(1)),Q(LAD(2)),Q(LAD(3)),
     +                    Q(LAD(4)),Q(LAD(5)),Q(LAD(6)),
     +                    Q(LAD(7)),Q(LAD(8)),Q(LAD(9)),Q(LAD(10)))
                  ENDIF
               ELSE
                  CALL CSJSUB(JAD,CHFORM,
     +                    Q(LAD(1)),Q(LAD(2)),Q(LAD(3)),
     +                    Q(LAD(4)),Q(LAD(5)),Q(LAD(6)),
     +                    Q(LAD(7)),Q(LAD(8)),Q(LAD(9)),Q(LAD(10)))
               ENDIF
  40        CONTINUE
         ENDIF
#endif
      ENDIF
*
  99  END
