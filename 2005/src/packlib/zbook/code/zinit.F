*
* $Id: zinit.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zinit.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZINIT(IZ,IW,NWORDS)
C
C     ******************************************************************
C     *                                                                *
C     *       INITIALISATION OF THE DATA STRUCTURE IZ                  *
C     *                                                                *
C     *       IZ(1)       JZ     =ZBOOK DIRECTORY                      *
C     *       IZ(JZ- 6)   JDESC BANK DESCRIPTOR                        *
C     *       IZ(JZ- 5)   JBUF BUFFER FOR DIRECT ACCESS                *
C     *       IZ(JZ- 4)   RESERVED FOR INTERNAL ZBOOK MANAGEMENT       *
C     *       IZ(JZ- 3)   RESERVED FOR INTERNAL ZBOOK MANAGEMENT       *
C     *       IZ(JZ- 2)   JDIR   =DISK DIRECTORY FOR LUNS              *
C     *       IZ(JZ- 1)   JLOC   =BANK FOR LOCAL POINTERS              *
C     *                                                                *
C     *       IZ(JZ+ 1)   NFREE  =FREE SPACE WITHOUT G.C               *
C     *       IZ(JZ+ 2)   NWORK  =LENGTH OF WORKING SPACE              *
C     *       IZ(JZ+ 3)   NBANK  =NUMBER OF IDS FOLLOWING IZ(1)        *
C     *       IZ(JZ+ 4)   LOUT   =LOGICAL UNIT FOR OUTPUT              *
C     *       IZ(JZ+ 5)   NUMBER OF WORDS IN DROPPED BANKS             *
C     *       IZ(JZ+ 6)   CURRENT ERROR NUMBER                         *
C     *       IZ(JZ+ 7)   NUMBER OF PUSHED BANKS                       *
C     *       IZ(JZ+ 8)   NUMBER OF CREATED BANKS                      *
C     *       IZ(JZ+ 9)   NUMBER OF GARBAGE COLLECTIONS                *
C     *       IZ(JZ+10)   MAX NUMBER OF ERROR MESSAGES PRINTED         *
C     *       IZ(JZ+11)   UPPER LIMIT FOR ZHINIT (HBOOK)               *
C     *       IZ(JZ+12)   LOWER ADDRESS RELATIVE TO IZ(1)              *
C     *       IZ(JZ+13)   HIGHEST      ""                              *
C     *       IZ(JZ+14)   CURRENT LENGTH OF THE STRUCTURE(NWORDS)      *
C     *       IZ(JZ+15)   IZLAST =POINTER FOR NEXT CREATED BANK        *
C     *       IZ(JZ+16)   -LOCF(IZ(1))                                 *
C     *       IZ(JZ+17)   INTERNAL ZBOOK FLAG                          *
C     *       IZ(JZ+18)   INTERNAL ZBOOK FLAG                          *
C     *       IZ(JZ+19)   ADDRESS OF RECOVERY ROUTINE                  *
C     *                                                                *
C     ******************************************************************
C
      COMMON/ZCFORM/NLFORM,JD(60)
      DIMENSION IZ(30),IW(1)
#if (defined(CERNLIB_SINGLE)||defined(CERNLIB_DOUBLE)||defined(CERNLIB_UNIVAC))&&(!defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_CDC))
      DATA LOUT/6/
#endif
#if defined(CERNLIB_CDC)
      DATA LOUT/L"OUTPUT"/
#endif
#if defined(CERNLIB_APOLLO)
      DATA LOUT/7/
#endif
#if defined(CERNLIB_BESM6)
      DATA LOUT/51/
#endif
#if defined(CERNLIB_PDP10)
      DATA LOUT/3/
#endif
      DATA NDIR/31/
C
C     ------------------------------------------------------------------
C
      CALL UMLOG('ZBOOK',' ')
C
      LOC0   = LOCF(IZ(1))
      NBANK  = LOCF(IW(1)) - LOC0 - 1
#if !defined(CERNLIB_BESM6)
      CALL VZERO(IZ(2),NBANK+NDIR+1)
#endif
#if defined(CERNLIB_BESM6)
      CALL UZERO(IZ,2,NBANK+NDIR+2)
#endif
      IZ(NBANK + 2) = NDIR
      IZ(1)  = NBANK + 9
      JZ     = IZ(1)
      IZ(JZ) = 21
      IZ(JZ + 1) = NWORDS - NBANK - NDIR - 2
      IZ(JZ + 3) = NBANK
      IZ(JZ + 4) = LOUT
      IZ(JZ + 10) = 100
      IZ(JZ + 11) = NWORDS
      IZ(JZ + 13) = NBANK
      IF (NBANK.LE.0)IZ(JZ + 13) = 1
      IZ(JZ + 14) = NWORDS
      IZ(JZ + 15) = NBANK + NDIR + 2
      IZ(JZ + 16) = - LOC0
      CALL UCTOH('*SYS',IZ(JZ + 20),4,4)
      IZ(JZ + 21) = 1
      IZ(JZ + 22) = 6
      IZ(JZ + 23) = 1
C--- ADDED BY HG
C    DUMMY CALL TO CREATE SYSTEM BANKS FOR FORMATS
      CALL ZBFORM(IZ,'*ZBF','X')
C---- Initialize HBOOK output unit
      CALL HOUTPU(LOUT)
      RETURN
      END
