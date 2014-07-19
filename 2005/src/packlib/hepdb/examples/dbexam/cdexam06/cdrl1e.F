*
* $Id: cdrl1e.F,v 1.1.1.1 1996/02/28 16:23:50 mclareni Exp $
*
* $Log: cdrl1e.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRL1E (ITIME)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDRL1E (ITIME)                                          *
*                                                                      *
*   Query of the Trigger Data Base for a given time                    *
*   as suggested by T. Hebecker                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Time for which the query refers                         *
*                                                                      *
*   Called by CEXAM06                                                  *
*                                                                      *
************************************************************************
*
#include "cstor6.inc"
#include "cuser6.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6)
      DIMENSION       KEY1(100), KEY2(100), KEY3(100), KEY4(100)
      DIMENSION       KEY5(100), KEY6(100), KEY7(100), KEY8(100)
      DIMENSION       MSK1(100), MSK2(100), MSK3(100), MSK4(100)
      DIMENSION       MSK5(100), MSK6(100), MSK7(100), MSK8(100)
      CHARACTER       COMNT*40, SERNB*8, STATU*4, OK*4, POLAR*4
      CHARACTER       CHOP1*20, CHOP2*20, CHOP3*20, CHOP4*20
      CHARACTER       CHOP5*20, CHOP6*20, CHOP7*20, CHOP8*20
      LOGICAL         FERA
      INTEGER         RESOU1, RESOU2
*
*     ------------------------------------------------------------------
*
* *** Unpack date and time
*
      CALL CDUPTS (ID1, IT1, ITIME, IRC)
*
* *** Find all modules (Topological number) in Rack # IRCK
* *** and perform further querries to these modules
*
      DO 100 IRCK = 1, 5
        WRITE (LPRTCU, 1001) IRCK
*
*  **   Camac number
*
        CALL VZERO (KEY5, 50)
        CALL VZERO (MSK5, 50)
        MSK5(NOFF2+3) = 1
        KEY5(NOFF2+3) = IRCK
        CHOP5 = 'SK'
        CALL CDUSEM ('//DBL3/TRIG/HARDCONF/BRACAM', LKTRCU(5), ITIME,
     +               MSK5, KEY5, CHOP5, IRC)
*
        IF (LKTRCU(5).EQ.0) THEN
          WRITE (LPRTCU, 1002)
          GO TO 100
        ENDIF
*
        NK5   = NZBANK (IDIVCU, LKTRCU(5))
        LKTRCU(9) = LKTRCU(5)
        DO 90 IK5 = 1, NK5
          IF (LKTRCU(9).EQ.0)             GO TO 95
          IBRA  = IQ(LKTRCU(9)+NOFF2+1)
          ICAM  = IQ(LKTRCU(9)+NOFF2+2)
          WRITE (LPRTCU, 1003) ICAM, IBRA
*
*  **     Module number
*
          CALL VZERO (MSK6, 50)
          CALL VZERO (KEY6, 50)
          MSK6(NOFF2+3) = 1
          MSK6(NOFF2+4) = 1
          KEY6(NOFF2+3) = IBRA
          KEY6(NOFF2+4) = ICAM
          CHOP6 = 'KS'
          CALL CDUSEM ('//DBL3/TRIG/HARDCONF/MODATT', LKTRCU(6), ITIME,
     +                 MSK6, KEY6, CHOP6, IRC)
*
          IF (LKTRCU(6).EQ.0)  THEN
            WRITE (LPRTCU, 1004)
            GO TO 90
          ENDIF
*
          NK6   = NZBANK (IDIVCU, LKTRCU(6))
          LKTRCU(10) = LKTRCU(6)
          DO 80 IK6 = 1, NK6
            IF (LKTRCU(10).EQ.0)          GO TO 85
            IMOD  = IQ(LKTRCU(10)+NOFF2+1)
            WRITE (LPRTCU, 1005) IMOD
*
*  **       Find the cable connections of that module
*  *        Output cables
*
            CALL VZERO (KEY7, 50)
            CALL VZERO (MSK7, 50)
            CHOP7 = 'SK'
            MSK7(NOFF2+1) = 1
            KEY7(NOFF2+1) = IMOD
            CALL CDUSEM ('//DBL3/TRIG/HARDCONF/CABCON', LKTRCU(7),
     +                   ITIME, MSK7, KEY7, CHOP7, IRC)
*
*  *         Print input cables (start-end, #, nicknumber and polarity)
*
            NK7  = NZBANK (IDIVCU, LKTRCU(7))
            IF (NK7.LE.0)  THEN
              WRITE (LPRTCU, 1006)
              GO TO 20
            ENDIF
            LKTRCU(11) = LKTRCU(7)
            DO 10 IK7 = 1, NK7
              IF (LKTRCU(11).EQ.0)        GO TO 20
              L11   = LKTRCU(11)
              CALL UHTOC (IQ(L11+NOFF2+8), 4, POLAR, 4)
              WRITE (LPRTCU, 1007) IQ(L11+NOFF2+2), IQ(L11+NOFF2+3),
     +                             IQ(L11+NOFF2+4), IQ(L11+NOFF2+5),
     +                             IQ(L11+NOFF2+6), IQ(L11+NOFF2+7),
     +                             POLAR
   10       LKTRCU(11) = LQ(LKTRCU(11))
*
   20       CALL CDFREE ('//DBL3/TRIG/HARDCONF/CABCON', LKTRCU(7), MSK7,
     +                   KEY7, CHOP7, IRC)
*
*  *        Input cables
*
            CALL VZERO (KEY7, 50)
            CALL VZERO (MSK7, 50)
            CHOP7 = 'SK'
            MSK7(NOFF2+4) = 1
            KEY7(NOFF2+4) = IMOD
            CALL CDUSEM ('//DBL3/TRIG/HARDCONF/CABCON', LKTRCU(7),
     +                   ITIME, MSK7, KEY7, CHOP7, IRC)
*
*  *         Print output cables (start-end, #, nicknumber and polarity)
*
            NK7  = NZBANK (IDIVCU, LKTRCU(7))
            IF (NK7.LE.0)  THEN
              WRITE (LPRTCU, 1008)
              GO TO 40
            ENDIF
            LKTRCU(11) = LKTRCU(7)
            DO 30 IK7 = 1, NK7
              IF (LKTRCU(11).EQ.0)        GO TO 40
              L11   = LKTRCU(11)
              CALL UHTOC (IQ(L11+NOFF2+8), 4, POLAR, 4)
              WRITE (LPRTCU, 1009) IQ(L11+NOFF2+5), IQ(L11+NOFF2+6),
     +                             IQ(L11+NOFF2+1), IQ(L11+NOFF2+2),
     +                             IQ(L11+NOFF2+3), IQ(L11+NOFF2+7),
     +                             POLAR
   30       LKTRCU(11) = LQ(LKTRCU(11))
*
   40       CALL CDFREE ('//DBL3/TRIG/HARDCONF/CABCON', LKTRCU(7), MSK7,
     +                   KEY7, CHOP7, IRC)
*
*  **       Find the serial number of that module
*
            CALL VZERO (KEY1, 50)
            CALL VZERO (MSK1, 50)
            CHOP1 = ' '
            MSK1(NOFF2+1) = 1
            KEY1(NOFF2+1) = IMOD
            CALL CDUSEM ('//DBL3/TRIG/TOPSER', LKTRCU(1), ITIME,
     +                   MSK1, KEY1, CHOP1, IRC)
*
*  **       Print serial # with the comment
*
            LDTRCU(1) = LQ(LKTRCU(1)-1)
            CALL UHTOC (IQ(LKTRCU(1)+NOFF2+2), 4, SERNB, 8)
            CALL UHTOC (IQ(LDTRCU(1)+1), 4, COMNT, 40)
            WRITE (LPRTCU, 1010) SERNB, KEY1(NOFF2+1), ID1, IT1, COMNT
*
            CALL CDFREE ('//DBL3/TRIG/TOPSER', LKTRCU(1), MSK1, KEY1,
     +                   CHOP1, IRC)
*
*  **       Print the properties of that particular module
*
            CALL VZERO (KEY2, 50)
            CALL VZERO (MSK2, 50)
            CALL UCTOH (SERNB, KEY2(NOFF2+1), 4, 8)
            CHOP2 = ' '
            MSK2(NOFF2+1) = 1
            MSK2(NOFF2+2) = 1
            CALL CDUSEM ('//DBL3/TRIG/MODPRP', LKTRCU(2), ITIME,
     +                   MSK2, KEY2, CHOP2, IRC)
*
            LDTRCU(2) = LQ(LKTRCU(2)-1)
            CALL UHTOC (IQ(LKTRCU(2)+NOFF2+3), 4, STATU, 4)
            ITYP   = IQ(LKTRCU(2)+NOFF2+4)
            FERA   = ITYP.EQ.5
            NOBITS = IQ(LKTRCU(2)+NOFF2+5)
            RESOU1 = IQ(LKTRCU(2)+NOFF2+6)
            RESOU2 = IQ(LKTRCU(2)+NOFF2+7)
            CALL UHTOC (IQ(LDTRCU(2)+1), 4, COMNT, 40)
*
            WRITE (LPRTCU, 1011) SERNB, STATU, ITYP, NOBITS, RESOU1,
     +                           RESOU2, COMNT
            IF (FERA) THEN
              WRITE (LPRTCU,1012)
              LDT    = LDTRCU(2) + 15
              DO 50 I = 1, 16
                CALL UHTOC (IQ(LDT+96+I), 4, OK, 4)
                WRITE (LPRTCU, 1013) IQ(LDT+I), Q(LDT+16+I), Q(LDT+32+I)
     +                             , Q(LDT+48+I), Q(LDT+64+I)
     +                             , Q(LDT+80+I), OK
   50         CONTINUE
            ENDIF
*
            CALL CDFREE ('//DBL3/TRIG/MODPRP', LKTRCU(2), MSK2, KEY2,
     +                   CHOP2, IRC)
*
*  **       Find all serial # which can satisfy the properties of module
*  **       First find the requested module properties
*
            CALL VZERO (KEY3, 50)
            CALL VZERO (MSK3, 50)
            CHOP3  = 'K'
            MSK3(NOFF2+1) = 1
            KEY3(NOFF2+1) = IMOD
*
            CALL CDUSEM ('//DBL3/TRIG/HARDCONF/MODATT', LKTRCU(3),
     +                   ITIME, MSK3, KEY3, CHOP3, IRC)
            ITYPE  = IQ(LKTRCU(3)+NOFF2+2)
            RESOU1 = IQ(LKTRCU(3)+NOFF2+9)
            RESOU2 = IQ(LKTRCU(3)+NOFF2+10)
*
            CALL CDFREE ('//DBL3/TRIG/HARDCONF/MODATT', LKTRCU(3), MSK3,
     +                   KEY3, CHOP3, IRC)
*
*  **       Next find all the serial numbers
*
            CHOP4  = 'VKS'
            STATU  = 'Y   '
            CALL VZERO (KEY4, 50)
            CALL VZERO (MSK4, 50)
            MSK4(NOFF2+3) = 1
            MSK4(NOFF2+4) = 1
            MSK4(NOFF2+6) = 1
            MSK4(NOFF2+7) = 1
            CALL UCTOH (STATU, KEY4(NOFF2+3), 4, 4)
            KEY4(NOFF2+4) = ITYPE
            KEY4(NOFF2+6) = RESOU1
            KEY4(NOFF2+7) = RESOU2
*
            CALL CDUSEM ('//DBL3/TRIG/MODPRP', LKTRCU(4), ITIME,
     +                   MSK4, KEY4, CHOP4, IRC)
            WRITE (LPRTCU, 1014) IMOD
            LKT    = LKTRCU(4)
            NS4    = NZBANK (IDIVCU, LKT)
            IF (NS4.GT.0)  THEN
   60         CALL UHTOC (IQ(LKT+NOFF2+1), 4, SERNB, 8)
              WRITE (LPRTCU, 1015) SERNB
              LKT    = LQ(LKT)
              IF (LKT.NE.0)               GO TO 60
            ENDIF
*
            CALL CDFREE ('//DBL3/TRIG/MODPRP', LKTRCU(4), MSK4, KEY4,
     +                   CHOP4, IRC)
#if defined(CERNLIB__DEBUG)
            CALL DZSHOW ('LKTRCU4 ', IDIVCU, LKTRCU(4), 'LVB', 0,0, 0,0)
#endif
*
   80     LKTRCU(10) = LQ(LKTRCU(10))
   85     CALL CDFREE ('//DBL3/TRIG/HARDCONF/MODATT', LKTRCU(6), MSK6,
     +                 KEY6, CHOP6, IRC)
*
   90   LKTRCU(9) = LQ(LKTRCU(9))
   95   CALL  CDFREE ('//DBL3/TRIG/HARDCONF/BRACAM', LKTRCU(5), MSK5,
     +                KEY5, CHOP5, IRC)
*
  100 CONTINUE
*
 1001 FORMAT (/5X,' ========== Rack Number : ',I5,' =========='//)
 1002 FORMAT (/10X,' +++ There is no Camac in this Rack !!! ')
 1003 FORMAT (/10X,' +++ Camac Number : ',I5,' Branch Number : ',I5
     +,' +++')
 1004 FORMAT (/15X,' --- There is NO Module in this Camac !!! ')
 1005 FORMAT (/15X,' --- Module Number : ',I5,' ---')
 1006 FORMAT (/10X,' ... There is NO output cable from this Module !!!')
 1007 FORMAT (/10X,' ... Cables from plugs :',I4,' -',I4,
     +        '   to module :',I8,' and plugs :',I4,' -',I4,I10,2X,A4)
 1008 FORMAT (/10X,' ... There is NO  input cable   to this Module !!!')
 1009 FORMAT (/10X,' ... Cables   to plugs :',I4,' -',I4,
     +        ' from module :',I8,' and plugs :',I4,' -',I4,I10,2X,A4)
 1010 FORMAT (/2X,'Module number : ',A8,' for topological no. :',I6
     +,' and validity : ',2I7,2X,A40)
 1011 FORMAT (/2X,'Properties of Module : ',A8,' Status  Modul type'
     +,'   Nobits  Res.Out1  Res.Out2     Comment'/36X,A4,4X,I4,3X,3I10
     +,5X,A40)
 1012 FORMAT (/20X,'  ADC - Properties  ',2X
     +,'Channel Pedest   R.M.S.    Slope  Intercept   Chi2  OK?'/)
 1013 FORMAT (45X,I2,5F9.3,2X,A4)
 1014 FORMAT (/20X,' --- All serial numbers for Module : ',I10,' ---'/)
 1015 FORMAT (25X,A8)
*                                                             END CDRL1E
  999 END
