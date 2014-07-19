*
* $Id: foput.F,v 1.1.1.1 1996/03/07 15:17:48 mclareni Exp $
*
* $Log: foput.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:48  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
 
#if !defined(CERNLIB_IBMRT)
      SUBROUTINE FOPUT(ITOTAL, IFLAG, IRETC)
#endif
#if defined(CERNLIB_IBMRT)
      SUBROUTINE FOPUT_(ITOTAL, IFLAG, IRETC)
#endif
      INTEGER IRETC, ITOTAL, IFLAG
 
*   Action  :  Modifies in the FATMEN file database the information
*              received in ITOTAL tuples of common block FOFILE.
*              Information is inserted as rows of tables GNAMES_,
*              FILES_, FXV_ and VOLUMES_.
*              If the file entry does not exist, take one of 2 actions:
*              include it (IFLAG=0) or return an error (IFLAG=1)
*              All generic names referred to in the common area must be
*              the same and provided in its complete format (without '*')
*
*   Receives: ITOTAL=> number of elements in the arrays of the COMMON
*                      area to be included in the database (<=10, but
*                      normally =1)
*             IFLAG => action when file entry not yet in FATMEN
*                      (=0, add new entry ; =1, error IRETC=-9996)
*
*   Returns : IRETC => return code (  0 => normal end. ITOTAL tuples
*                                          written from FOFILE into the
*                                          database
*                                  -9999=> generic name is not the same
*                                          for all tuples
*                                  -9998=> SQL warning. See FOR006
*                                  -9997=> //database/experiment does
*                                          not exist
*                                  -9996=> entry file to be modified
*                                          does not exist (IFLAG=1)
*                                  -9995=> entry file to be added
*                                          already exists (IFLAG=-1)
*                                  -6079 to
*                                  -0001=> SQL errors, IRETC=SQLCDE )
*             ITOTAL=> when IRETC=-9996, tuple where error occured
*
*   Responsible : Luigino Palermo (PALERMO@VXCERN,CERNVM)
*
*   Changes history:
*
*      DATE                    MODIFICATIONS
*----------------------------------------------------------------------
*
*  13/07/89        Inclusion of creator account (CREACC) and 10 system
*                  words (SYSWDx); use of Patchy to manage code
*  15/09/89        Modification to allow more than one generic name
*                  pointing to the same file (aliases for generic names)
*  26/09/89        Recodification, using dynamic-defined statements.
*                  Method 1 (EXECUTE IMMEDIATE) for LOCK, method 3
*                  (PREPARE, DECLARE, OPEN & FETCH) for SELECT and
*  17/01/90        Location is no more considered to check existence of
*                  a tape file entry. Default action of FOPUT now is to
*                  modify existing entries. If entry is not in FATMEN,
*                  action is defined according to IFLAG value
*  29/01/90        Correction of error when modifying a file: it was
*                  including an entry in FXV_ even if was already there
*                  File seq # is NOT NULL, so variable FLSQNI removed
*  01/02/90        Restructuring of the routine due to consecutive errors
*                  of logic, after having changed the default operation
*                  to modification and included parameter IFLAG
 
#if defined(CERNLIB_IBMRT)
#include "fatmen/fofilex.inc"
#endif
#if !defined(CERNLIB_IBMRT)
#include "fatmen/fofile.inc"
#endif
#include "fatmen/fovars.inc"
*      To get IDEBFA for debug
#include "fatmen/fatbank.inc"
 
*...General variables
 
      INTEGER I, J, I1, I2, I3, LEXP
      LOGICAL OLDGN, OLDFILE
      EXTERNAL CLTOU
      CHARACTER*6 CMD
*
*...Variables for Oracle...............................................
*
      EXEC SQL BEGIN DECLARE SECTION
 
      CHARACTER*8 DBASE, EXPER
      CHARACTER*100 STMT1, STMT2, STMT3, STMT5, STMT6, STMT7, STMT9,
     1              STMT11, STMT12, STMT16
      CHARACTER*150 STMT8, STMT14, STMT15
      CHARACTER*300 STMT4
      CHARACTER*1000 STMT10
      CHARACTER*1500 STMT13
#if defined(CERNLIB_SQLDS)
      COMMON/STMT40/LSTMT04,STMT4
      CHARACTER*200 STMT04
      EQUIVALENCE   (LSTMT04,STMT04)
      COMMON/STMT0A/LSTMT10,STMT10
      CHARACTER*200 STMT0A
      EQUIVALENCE   (LSTMT10,STMT0A)
      COMMON/STMT0A/LSTMT13,STMT13
      CHARACTER*200 STMT0D
      EQUIVALENCE   (LSTMT13,STMT0D)
#endif
 
*...Auxiliary host variables to hold internal identifiers of rows
 
      INTEGER GNID, FILEID, VOLID
 
*...Auxiliary host vars for GNAMES_ table
 
      CHARACTER*240 GENAM2
 
*...Auxiliary host vars for FILES_ table
*   (USRWx2 instead of USRWDx2, sorry!)
*   (SYSWx2 instead of SYSWDx2, sorry again!)
 
      INTEGER CPLV2,  LOCA2,  SRTRE2, ENDRE2,
     1        SRTBL2, ENDBL2, RECLG2, BLKLG2,
     2        PROTE2, USRW02, USRW12, USRW22,
     3        USRW32, USRW42, USRW52, USRW62,
     4        USRW72, USRW82, USRW92, SYSW02,
     5        SYSW12, SYSW22, SYSW32, SYSW42,
     6        SYSW52, SYSW62, SYSW72, SYSW82,
     7        SYSW92, FLSQN2, VLSQN2
 
      CHARACTER HNAM2*8,   FNAM2*240, HTYP2*16,  OPSY2*12,
     1          FFORM2*4,  USRFM2*4,  RECFM2*4,  CREDA2*15,
     2          CATDA2*15, ACSDA2*15, CRENA2*8,  CREAC2*8,
     3          CRENO2*8,  CREJO2*8,  COMMT2*80, MEDI2*1,
     4          ACTIV2*1
 
*...Auxiliary hots vars for VOLUMES_ table
 
      INTEGER PRE2, DEN2
 
      CHARACTER VI2*6, VS2*6
 
*...Indicator variables for all possible NULL columns
 
      INTEGER*2 HTYPI,  OPSYI,  USRFMI, SRTREI, ENDREI, SRTBLI, ENDBLI,
     1          RECFMI, RECLGI, BLKLGI, CREDAI, CATDAI, ACSDAI, CRENAI,
     2          CRENOI, CREJOI, PROTEI, USRW0I, USRW1I, USRW2I, USRW3I,
     3          USRW4I, USRW5I, USRW6I, USRW7I, USRW8I, USRW9I, SYSW0I,
     4          SYSW1I, SYSW2I, SYSW3I, SYSW4I, SYSW5I, SYSW6I, SYSW7I,
     5          SYSW8I, SYSW9I, COMMTI, MEDII,  VLSQNI, PREI,
     6          DENI,   CREACI
 
      EXEC SQL END DECLARE SECTION
*......................................................................
 
 
*
*...SQL common area
*
      EXEC SQL INCLUDE SQLCA.FOR
 
*...Verifies if generic name is the same for all files to be included
 
      DO 10 I=1,ITOTAL-1
        IF (GENAME(I).NE.GENAME(I+1)) THEN
          IF(IDEBFA.GT.2)
     +    PRINT *,'FOPUT. Generic name must be the same for all files'
          IRETC = -9999
          RETURN
        ENDIF
   10 CONTINUE
 
      GENAM2 = GENAME(1)
 
*...  Converts lower to upper case in the generic name
 
      CALL CLTOU (GENAM2)
 
*...  I1, I2, and I3 mark the limits of first 2 parts of generic name
 
      I1 = INDEX (GENAM2, '//')
      I2 = INDEX (GENAM2(I1+2:), '/')
      I3 = INDEX (GENAM2(I1+I2+2:), '/')
      IF ((I1.LE.0).OR.(I2.LE.1).OR.(I3.LE.1)) GOTO 1300
 
*...  Parse generic name and exclude //database/experiment/ prologue
*     (in the tables of an experiment do not contain the database and
*     experiment names)
 
      DBASE = GENAM2(I1+2:I1+I2)
      EXPER = GENAM2(I1+I2+2:I1+I2+I3)
      LEXP = INDEX (EXPER, ' ') - 1
      GENAM2 = GENAM2(I1+I2+I3+2:)
 
      EXEC SQL WHENEVER SQLWARNING GOTO 1100
      EXEC SQL WHENEVER SQLERROR   GOTO 1200
 
*...Locks FATMEN table to avoid parallel writing while reading it
 
      EXEC SQL LOCK TABLE FATMEN IN SHARE MODE
 
*...Looks for database and experiment in FATMEN table (if doesn't exist,
*...returns error -9997)
 
      EXEC SQL WHENEVER NOT FOUND GOTO 1300
 
      EXEC SQL SELECT * INTO :DBASE, :EXPER  FROM FATMEN
     1         WHERE DATABASE =:DBASE AND EXPERIMENT =:EXPER
 
*...Locks all tables to be modified for exclusive access
 
      STMT1 = 'LOCK TABLE GNAMES_' // EXPER(1:LEXP) // ', FILES_' //
     1         EXPER(1:LEXP) // ', FXV_' // EXPER(1:LEXP) //
     2        ', VOLUMES_' // EXPER(1:LEXP) // ' IN EXCLUSIVE MODE'
 
      CMD = 'STMT1'
      EXEC SQL EXECUTE IMMEDIATE :STMT1
 
*...Looks for generic name in GNAMES_ table(if already exists drops
*...insert step)
 
      EXEC SQL WHENEVER NOT FOUND CONTINUE
 
      STMT2 = 'SELECT GN# FROM GNAMES_' // EXPER(1:LEXP) //
     1        ' WHERE GNAME = :GENAM2'
 
      CMD = 'STMT2'
      EXEC SQL PREPARE S2 FROM :STMT2
      EXEC SQL DECLARE C2 CURSOR FOR S2
      EXEC SQL OPEN C2 USING :GENAM2
      EXEC SQL FETCH C2 INTO :GNID
 
      OLDGN = .FALSE.
 
      IF(IDEBFA.GT.2)
     +  PRINT *,'FOPUT. Return code from select on GNAME = ',SQLERD(3),
     +          ' IFLAG = ',IFLAG
 
      IF (SQLERD(3).EQ.0) THEN
 
         IF (IFLAG.EQ.1) THEN
*...        If IFLAG is set, then generic name should exist !
            GOTO 1400
         ELSE
 
*...       If generic name does not exist, takes greatest GN#,
*...       increments by 1 and inserts new row in GNAMES_ table.
 
           STMT3 = 'SELECT MAX(GN#) FROM GNAMES_' // EXPER(1:LEXP)
 
           CMD = 'STMT3'
           EXEC SQL PREPARE S3 FROM :STMT3
           EXEC SQL DECLARE C3 CURSOR FOR S3
           EXEC SQL OPEN C3
           EXEC SQL FETCH C3 INTO :GNID
 
           GNID = GNID + 1
           STMT9 = 'INSERT INTO GNAMES_' // EXPER(1:LEXP)
     1           // ' (GNAME, GN#) VALUES (:GENAM2, :GNID)'
 
           CMD = 'STMT9'
           EXEC SQL PREPARE S9 FROM :STMT9
           EXEC SQL EXECUTE S9 USING :GENAM2, :GNID
         ENDIF
 
      ELSE
         OLDGN = .TRUE.
      ENDIF
 
*...  Prepares dynamic-defined statements to be used inside the loop
*     (DO 50 J=1,ITOTAL) and declares cursors when necessary (SELECTs)
 
      STMT4 = 'SELECT FILES_' // EXPER(1:LEXP) // '.FILE#' //
     1        ' FROM VOLUMES_' // EXPER(1:LEXP) // ', FXV_' //
     2          EXPER(1:LEXP) // ', FILES_' // EXPER(1:LEXP) //
     3        ' WHERE FILES_' // EXPER(1:LEXP) // '.FILE# = ' //
     4        'FXV_' // EXPER(1:LEXP) // '.FILE# AND FXV_' //
     5          EXPER(1:LEXP) //
     6        '.VOL# = VOLUMES_' // EXPER(1:LEXP) // '.VOL# AND ' //
     7        'VID = :VI2 AND VSN = :VS2 AND FILESEQ# = :FLSQN2' //
     8        ' AND MEDIATYPE = :MEDI2' //
     9        ' AND GN# = :GNID AND ACTIVE = ''Y'''
 
#if defined(CERNLIB_ORACLE)
      EXEC SQL PREPARE S4 FROM :STMT4
#endif
#if defined(CERNLIB_SQLDS)
      EXEC SQL PREPARE S4 FROM :STMT04
#endif
      EXEC SQL DECLARE C4 CURSOR FOR S4
 
      STMT5 = 'SELECT MAX(FILE#) FROM FILES_' // EXPER(1:LEXP)
 
      EXEC SQL PREPARE S5 FROM :STMT5
      EXEC SQL DECLARE C5 CURSOR FOR S5
 
      STMT6 = 'SELECT VOL# FROM VOLUMES_' // EXPER(1:LEXP) //
     1        ' WHERE VSN = :VS2 AND  VID = :VI2'
 
      EXEC SQL PREPARE S6 FROM :STMT6
      EXEC SQL DECLARE C6 CURSOR FOR S6
 
      STMT7 = 'SELECT MAX(VOL#) FROM VOLUMES_' // EXPER(1:LEXP)
 
      EXEC SQL PREPARE S7 FROM :STMT7
      EXEC SQL DECLARE C7 CURSOR FOR S7
 
      STMT8 = 'SELECT FILE# FROM FILES_' // EXPER(1:LEXP) //
     1        ' WHERE HOSTNAME = :HNAM2 AND FULLNAME = :FNAM2' //
     2        ' AND MEDIATYPE = :MEDI2' //
     3        ' AND GN# = :GNID AND ACTIVE = ''Y'''
 
      EXEC SQL PREPARE S8 FROM :STMT8
      EXEC SQL DECLARE C8 CURSOR FOR S8
 
      STMT10 = 'INSERT INTO FILES_' //
     1          EXPER(1:LEXP) // ' VALUES (:FILEID,' //
     2         ' :GNID, :CPLV2, :LOCA2, :HNAM2, :FNAM2,' //
     3         ' :HTYP2:HTYPI,   :OPSY2:OPSYI,   :FFORM2,' //
     4         ' :USRFM2:USRFMI, :SRTRE2:SRTREI, :ENDRE2:ENDREI,' //
     5         ' :SRTBL2:SRTBLI, :ENDBL2:ENDBLI, :RECFM2:RECFMI,' //
     6         ' :RECLG2:RECLGI, :BLKLG2:BLKLGI,' //
     7         ' TO_DATE(:CREDA2:CREDAI,''DD-MON-YY HH24:MI''),' //
     8         ' TO_DATE(:CATDA2:CATDAI,''DD-MON-YY HH24:MI''),' //
     9         ' TO_DATE(:ACSDA2:ACSDAI,''DD-MON-YY HH24:MI''),' //
     A         ' :ACTIV2, :CRENA2:CRENAI,' //
     B         ' :CREAC2:CREACI, :CRENO2:CRENOI, :CREJO2:CREJOI,' //
     C         ' :PROTE2:PROTEI, :USRW02:USRW0I, :USRW12:USRW1I,' //
     D         ' :USRW22:USRW2I, :USRW32:USRW3I, :USRW42:USRW4I,' //
     E         ' :USRW52:USRW5I, :USRW62:USRW6I, :USRW72:USRW7I,' //
     F         ' :USRW82:USRW8I, :USRW92:USRW9I, :SYSW02:SYSW0I,' //
     G         ' :SYSW12:SYSW1I, :SYSW22:SYSW2I, :SYSW32:SYSW3I,' //
     H         ' :SYSW42:SYSW4I, :SYSW52:SYSW5I, :SYSW62:SYSW6I,' //
     I         ' :SYSW72:SYSW7I, :SYSW82:SYSW8I, :SYSW92:SYSW9I,' //
     J         ' :COMMT2:COMMTI, :MEDI2:MEDII)'
 
#if defined(CERNLIB_ORACLE)
      EXEC SQL PREPARE S10 FROM :STMT10
#endif
#if defined(CERNLIB_SQLDS)
      EXEC SQL PREPARE S10 FROM :STMT0A
#endif
 
      STMT11 = 'INSERT INTO VOLUMES_' // EXPER(1:LEXP) //
     1         ' VALUES (:VOLID, :VS2, :VI2, :PRE2:PREI, :DEN2:DENI)'
 
      EXEC SQL PREPARE S11 FROM :STMT11
 
      STMT12 = 'INSERT INTO FXV_' // EXPER(1:LEXP) //
     1      ' VALUES (:FILEID, :FLSQN2, :VOLID, :VLSQN2:VLSQNI)'
 
      EXEC SQL PREPARE S12 FROM :STMT12
 
      STMT13 = 'UPDATE FILES_' // EXPER(1:LEXP) //
     1 ' SET GN#=:GNID, COPYLEVEL=:CPLV2, LOCATION=:LOCA2,' //
     2 ' HOSTNAME=:HNAM2, FULLNAME=:FNAM2,' //
     3 ' HOSTTYPE=:HTYP2:HTYPI, OPERSYS=:OPSY2:OPSYI,' //
     4 ' FILEFORMAT=:FFORM2, USERFORMAT=:USRFM2:USRFMI,' //
     5 ' STARTREC#=:SRTRE2:SRTREI, ENDREC#=:ENDRE2:ENDREI,' //
     6 ' STARTBLK#=:SRTBL2:SRTBLI, ENDBLK#=:ENDBL2:ENDBLI,' //
     7 ' RECFORMAT=:RECFM2:RECFMI, RECLENGTH=:RECLG2:RECLGI,' //
     8 ' BLKLENGTH=:BLKLG2:BLKLGI,' //
     9 ' CREATION=TO_DATE(:CREDA2:CREDAI,''DD-MON-YY HH24:MI''),' //
     A ' CATALOGATION=TO_DATE(:CATDA2:CATDAI,''DD-MON-YY HH24:MI''),'//
     B ' LASTACCESS=TO_DATE(:ACSDA2:ACSDAI,''DD-MON-YY HH24:MI''),'  //
     C ' ACTIVE=:ACTIV2, CREATORNAME=:CRENA2:CRENAI,' //
     D ' CREATORACCOUNT=:CREAC2:CREACI, CREATORNODE=:CRENO2:CRENOI,' //
     E ' CREATORJOB=:CREJO2:CREJOI, PROTECTION=:PROTE2:PROTEI,' //
     F ' USERWORD0=:USRW02:USRW0I,  USERWORD1=:USRW12:USRW1I,'  //
     G ' USERWORD2=:USRW22:USRW2I,  USERWORD3=:USRW32:USRW3I,'  //
     H ' USERWORD4=:USRW42:USRW4I,  USERWORD5=:USRW52:USRW5I,'  //
     I ' USERWORD6=:USRW62:USRW6I,  USERWORD7=:USRW72:USRW7I,'  //
     J ' USERWORD8=:USRW82:USRW8I,  USERWORD9=:USRW92:USRW9I,'  //
     K ' SYSWORD0=:SYSW02:SYSW0I,   SYSWORD1=:SYSW12:SYSW1I,'   //
     L ' SYSWORD2=:SYSW22:SYSW2I,   SYSWORD3=:SYSW32:SYSW3I,'   //
     M ' SYSWORD4=:SYSW42:SYSW4I,   SYSWORD5=:SYSW52:SYSW5I,'   //
     N ' SYSWORD6=:SYSW62:SYSW6I,   SYSWORD7=:SYSW72:SYSW7I,'   //
     O ' SYSWORD8=:SYSW82:SYSW8I,   SYSWORD9=:SYSW92:SYSW9I,  ' //
     P ' COMMENTS=:COMMT2:COMMTI,   MEDIATYPE=:MEDI2:MEDII ' //
     Q ' WHERE FILE#=:FILEID'
 
#if defined(CERNLIB_ORACLE)
      EXEC SQL PREPARE S13 FROM :STMT13
#endif
#if defined(CERNLIB_SQLDS)
      EXEC SQL PREPARE S13 FROM :STMT0D
#endif
 
      STMT14= 'SELECT FILE# FROM FXV_' // EXPER(1:LEXP) //
     1        ' WHERE FILE# = :FILEID AND FILESEQ# = :FLSQN2' //
     2        ' AND VOL# = :VOLID'
 
      EXEC SQL PREPARE S14 FROM :STMT14
      EXEC SQL DECLARE C14 CURSOR FOR S14
 
      STMT15 = 'UPDATE VOLUMES_' // EXPER(1:LEXP) //
     1         ' SET VIDPREFIX = :PRE2:PREI, DENSITY = :DEN2:DENI' //
     2         ' WHERE VSN = :VS2 AND VID = :VI2'
 
      EXEC SQL PREPARE S15 FROM :STMT15
 
      STMT16 = 'SELECT FILE# FROM FILES_' // EXPER(1:LEXP) //
     1        ' WHERE MEDIATYPE = :MEDI2' //
     2        ' AND GN# = :GNID AND ACTIVE = ''Y'''
 
      EXEC SQL PREPARE S16 FROM :STMT16
      EXEC SQL DECLARE C16 CURSOR FOR S16
 
*...NB: Stores every array bind variable to be used in SQL statement in
*...    a temporary variable with same name less last char, plus "2"
*...    (PCC precompiler DOES NOT accept arrays elements !)
 
      DO 50 J=1,ITOTAL
 
         MEDI2 = MEDIA(J)
         CALL CLTOU (MEDI2)
         OLDFILE = .FALSE.
 
         IF (MEDI2.EQ.'D') THEN
 
            IF(IDEBFA.GT.2)
     +      PRINT *,'FOPUT. Media is disk, check on ',
     +               'HNAME,FNAME,MEDIA and GN#'
 
*...        Assigns values to temporary variables to be used in SELECT
*           of FILES_ table
 
            HNAM2 = HNAME(J)
            CALL CLTOU (HNAM2)
 
            FNAM2 = FNAME(J)
            CALL CLTOU (FNAM2)
 
            CMD = 'STMT8'
            EXEC SQL OPEN C8 USING :HNAM2, :FNAM2, :MEDI2, :GNID
            EXEC SQL FETCH C8 INTO :FILEID
 
         ELSE
 
*...        Assigns values to temporary variables to be used in SELECT
*           (if file on tape, columns to identify it uniquely come from
*           FILES_, FXV_ and VOLUMES_ tables)
 
            IF(IDEBFA.GT.2)
     +      PRINT *,'FOPUT. Media is tape, check on ',
     +               'VSN,VID,FSEQ,MEDIA and GN#'
 
 
            VI2 = VID(J)
            CALL CLTOU (VI2)
 
            VS2 = VSN(J)
            CALL CLTOU (VS2)
 
            FLSQN2 = FLSQNO(J)
 
            CMD = 'STMT4'
            EXEC SQL OPEN C4 USING :VI2, :VS2, :FLSQN2,
     1                             :MEDI2, :GNID
            EXEC SQL FETCH C4 INTO :FILEID
 
         ENDIF
 
*...     Non-existence of file entry to be modified is error only
*        if IFLAG is set
 
      IF(IDEBFA.GT.2)
     +  PRINT *,'FOPUT. Return code from select on FILE  = ',SQLERD(3),
     +          ' IFLAG = ',IFLAG
 
         IF (SQLERD(3).NE.0) OLDFILE = .TRUE.
         IF ((.NOT.OLDFILE).AND.(IFLAG.EQ.1))  GOTO 1400
*
*    If IFLAG = -1, error if file already exists
*
         IF ((OLDFILE)     .AND.(IFLAG.EQ.-1)) GOTO 1500
 
*...     Assigns values to temporary variables to be used in INSERT
*        and UPDATE commands on FILES_, FXV_ and VOLUMES_ tables
*        (PCC does not accept array elements!)
 
         LOCA2  = LOCAT(J)
         CPLV2 = CPLVL(J)
         HNAM2 = HNAME(J)
         CALL CLTOU (HNAM2)
         FNAM2 = FNAME(J)
         CALL CLTOU (FNAM2)
         FFORM2 = FFORMT(J)
         CALL CLTOU (FFORM2)
         ACTIV2 = ACTIVE(J)
         CALL CLTOU (ACTIV2)
 
         HTYP2  = HTYPE(J)
         IF (HTYP2.EQ.' ') THEN
             HTYPI = -1
         ELSE
             CALL CLTOU (HTYP2)
         ENDIF
 
         OPSY2  = OPSYS(J)
         IF (OPSY2.EQ.' ') THEN
             OPSYI = -1
         ELSE
             CALL CLTOU (OPSY2)
         ENDIF
 
         USRFM2 = USRFMT(J)
         IF (USRFM2.EQ.' ') THEN
             USRFMI = -1
         ELSE
             CALL CLTOU (USRFM2)
         ENDIF
 
         SRTRE2 = SRTREC(J)
         IF (SRTRE2.EQ.-1)  SRTREI = -1
 
         ENDRE2 = ENDREC(J)
         IF (ENDRE2.EQ.-1)  ENDREI = -1
 
         SRTBL2 = SRTBLK(J)
         IF (SRTBL2.EQ.-1)  SRTBLI = -1
 
         ENDBL2 = ENDBLK(J)
         IF (ENDBL2.EQ.-1)  ENDBLI = -1
 
         RECFM2 = RECFMT(J)
         IF (RECFM2.EQ.' ') THEN
             RECFMI = -1
         ELSE
             CALL CLTOU (RECFM2)
         ENDIF
 
         RECLG2 = RECLGH(J)
         IF (RECLG2.EQ.-1)  RECLGI = -1
 
         BLKLG2 = BLKLGH(J)
         IF (BLKLG2.EQ.-1) BLKLGI = -1
 
         CREDA2 = CREDAT(J)
         IF (CREDA2.EQ.' ') THEN
             CREDAI = -1
         ELSE
             CALL CLTOU (CREDA2)
         ENDIF
 
         CATDA2 = CATDAT(J)
         IF (CATDA2.EQ.' ') THEN
             CATDAI = -1
         ELSE
             CALL CLTOU (CATDA2)
         ENDIF
 
         ACSDA2 = ACSDAT(J)
         IF (ACSDA2.EQ.' ') THEN
             ACSDAI = -1
         ELSE
             CALL CLTOU (ACSDA2)
         ENDIF
 
         CRENA2 = CRENAM(J)
         IF (CRENA2.EQ.' ') THEN
             CRENAI = -1
         ELSE
             CALL CLTOU (CRENA2)
         ENDIF
 
         CREAC2 = CREACC(J)
         IF (CREAC2.EQ.' ') THEN
             CREACI = -1
         ELSE
             CALL CLTOU (CREAC2)
         ENDIF
 
         CRENO2 = CRENOD(J)
         IF (CRENO2.EQ.' ') THEN
             CRENOI = -1
         ELSE
             CALL CLTOU (CRENO2)
         ENDIF
 
         CREJO2 = CREJOB(J)
         IF (CREJO2.EQ.' ') THEN
             CREJOI = -1
         ELSE
             CALL CLTOU (CREJO2)
         ENDIF
 
         PROTE2 = PROTEC(J)
         IF (PROTE2.EQ.-1)  PROTEI = -1
 
         USRW02 = USRWD0(J)
         IF (USRW02.EQ.-1)  USRW0I = -1
 
         USRW12 = USRWD1(J)
         IF (USRW12.EQ.-1)  USRW1I = -1
 
         USRW22 = USRWD2(J)
         IF (USRW22.EQ.-1)  USRW2I = -1
 
         USRW32 = USRWD3(J)
         IF (USRW32.EQ.-1)  USRW3I = -1
 
         USRW42 = USRWD4(J)
         IF (USRW42.EQ.-1)  USRW4I = -1
 
         USRW52 = USRWD5(J)
         IF (USRW52.EQ.-1) USRW5I = -1
 
         USRW62 = USRWD6(J)
         IF (USRW62.EQ.-1) USRW6I = -1
 
         USRW72 = USRWD7(J)
         IF (USRW72.EQ.-1) USRW7I = -1
 
         USRW82 = USRWD8(J)
         IF (USRW82.EQ.-1) USRW8I = -1
 
         USRW92 = USRWD9(J)
         IF (USRW92.EQ.-1) USRW9I = -1
 
         SYSW02 = SYSWD0(J)
         IF (SYSW02.EQ.-1) SYSW0I = -1
 
         SYSW12 = SYSWD1(J)
         IF (SYSW12.EQ.-1) SYSW1I = -1
 
         SYSW22 = SYSWD2(J)
         IF (SYSW22.EQ.-1) SYSW2I = -1
 
         SYSW32 = SYSWD3(J)
         IF (SYSW32.EQ.-1) SYSW3I = -1
 
         SYSW42 = SYSWD4(J)
         IF (SYSW42.EQ.-1) SYSW4I = -1
 
         SYSW52 = SYSWD5(J)
         IF (SYSW52.EQ.-1) SYSW5I = -1
 
         SYSW62 = SYSWD6(J)
         IF (SYSW62.EQ.-1) SYSW6I = -1
 
         SYSW72 = SYSWD7(J)
         IF (SYSW72.EQ.-1) SYSW7I = -1
 
         SYSW82 = SYSWD8(J)
         IF (SYSW82.EQ.-1) SYSW8I = -1
 
         SYSW92 = SYSWD9(J)
         IF (SYSW92.EQ.-1) SYSW9I = -1
 
         COMMT2 = COMMTS(J)
         IF (COMMT2.EQ.' ') THEN
             COMMTI = -1
         ELSE
             CALL CLTOU (COMMT2)
         ENDIF
 
         IF (MEDI2.NE.'D') THEN
 
*...         Assigns values to temporary vars to be used in INSERT/
*            UPDATE in FILES_, VOLUMES_ tables
 
             PRE2 = PREF(J)
             IF (PRE2.EQ.-1) PREI = -1
 
             DEN2 = DENS(J)
             IF (DEN2.EQ.-1) DENI = -1
 
*...         Assigns values to temporary vars to be used in INSERT
*            UPDATE FXV_ table (always insert, because FILEID is new)
 
             FLSQN2 = FLSQNO(J)
 
             VLSQN2 = VLSQNO(J)
             IF (VLSQN2.EQ.-1)  VLSQNI = -1
 
         ENDIF
 
         IF (OLDFILE.AND.OLDGN) THEN
 
*...        Generic name and file already exist, so perform an UPDATE
 
            IF(IDEBFA.GT.2) PRINT *,'FOPUT. Updating existing entry.'
 
            CMD = 'STMT13'
            EXEC SQL EXECUTE S13 USING
     1            :GNID, :CPLV2, :LOCA2, :HNAM2, :FNAM2,
     2            :HTYP2:HTYPI,   :OPSY2:OPSYI,   :FFORM2,
     3            :USRFM2:USRFMI, :SRTRE2:SRTREI, :ENDRE2:ENDREI,
     4            :SRTBL2:SRTBLI, :ENDBL2:ENDBLI, :RECFM2:RECFMI,
     5            :RECLG2:RECLGI, :BLKLG2:BLKLGI,
     6            :CREDA2:CREDAI, :CATDA2:CATDAI,
     7            :ACSDA2:ACSDAI, :ACTIV2,        :CRENA2:CRENAI,
     8            :CREAC2:CREACI, :CRENO2:CRENOI, :CREJO2:CREJOI,
     9            :PROTE2:PROTEI, :USRW02:USRW0I, :USRW12:USRW1I,
     A            :USRW22:USRW2I, :USRW32:USRW3I, :USRW42:USRW4I,
     B            :USRW52:USRW5I, :USRW62:USRW6I, :USRW72:USRW7I,
     C            :USRW82:USRW8I, :USRW92:USRW9I, :SYSW02:SYSW0I,
     D            :SYSW12:SYSW1I, :SYSW22:SYSW2I, :SYSW32:SYSW3I,
     E            :SYSW42:SYSW4I, :SYSW52:SYSW5I, :SYSW62:SYSW6I,
     F            :SYSW72:SYSW7I, :SYSW82:SYSW8I, :SYSW92:SYSW9I,
     G            :COMMT2:COMMTI, :MEDI2:MEDII,   :FILEID
 
*...        Modify also VOLUMES_ if tape file (new prefix or density)
 
            IF (MEDI2.NE.'D') THEN
                CMD = 'STMT15'
                EXEC SQL EXECUTE S15 USING :PRE2:PREI,
     1                                     :DEN2:DENI, :VS2, :VI2
            ENDIF
 
         ELSE
 
*...        There is a new generic name (new GNID) or a new file (new
*           FILEID), or both, but the former was already included
 
            IF (.NOT.OLDFILE) THEN
 
               IF (MEDI2.EQ.'D') THEN
 
*...              Disk file not in FILES_ table. Gets greatest
*                 FILE#, increments by 1 to be key of new row
 
                  CMD = 'STMT5'
                  EXEC SQL OPEN C5
                  EXEC SQL FETCH C5 INTO :FILEID
                  FILEID = FILEID + 1
 
                  CMD = 'STMT10'
                  EXEC SQL EXECUTE S10 USING :FILEID,
     1               :GNID, :CPLV2, :LOCA2, :HNAM2, :FNAM2,
     2               :HTYP2:HTYPI,   :OPSY2:OPSYI,   :FFORM2,
     3               :USRFM2:USRFMI, :SRTRE2:SRTREI, :ENDRE2:ENDREI,
     4               :SRTBL2:SRTBLI, :ENDBL2:ENDBLI, :RECFM2:RECFMI,
     5               :RECLG2:RECLGI, :BLKLG2:BLKLGI,
     6               :CREDA2:CREDAI, :CATDA2:CATDAI,
     7               :ACSDA2:ACSDAI, :ACTIV2,        :CRENA2:CRENAI,
     8               :CREAC2:CREACI, :CRENO2:CRENOI, :CREJO2:CREJOI,
     9               :PROTE2:PROTEI, :USRW02:USRW0I, :USRW12:USRW1I,
     A               :USRW22:USRW2I, :USRW32:USRW3I, :USRW42:USRW4I,
     B               :USRW52:USRW5I, :USRW62:USRW6I, :USRW72:USRW7I,
     C               :USRW82:USRW8I, :USRW92:USRW9I, :SYSW02:SYSW0I,
     D               :SYSW12:SYSW1I, :SYSW22:SYSW2I, :SYSW32:SYSW3I,
     E               :SYSW42:SYSW4I, :SYSW52:SYSW5I, :SYSW62:SYSW6I,
     F               :SYSW72:SYSW7I, :SYSW82:SYSW8I, :SYSW92:SYSW9I,
     G               :COMMT2:COMMTI, :MEDI2:MEDII
 
               ELSE
 
*...              If it is a tape file, maybe FILES_ entry is already
*                 there (check without considering tape attributes)
 
                  CMD = 'STMT16'
                  EXEC SQL OPEN C16 USING :MEDI2, :GNID
                  EXEC SQL FETCH C16 INTO :FILEID
 
                  IF (SQLERD(3).EQ.0) THEN
 
*...                 Tape file not in FILES_ table. Gets greatest
*                    FILE#, increments by 1 to be key of new row
 
                     CMD = 'STMT5'
                     EXEC SQL OPEN C5
                     EXEC SQL FETCH C5 INTO :FILEID
 
                     FILEID = FILEID + 1
 
                     CMD = 'STMT10'
                     EXEC SQL EXECUTE S10 USING :FILEID,
     1                  :GNID, :CPLV2, :LOCA2, :HNAM2, :FNAM2,
     2                  :HTYP2:HTYPI,   :OPSY2:OPSYI,   :FFORM2,
     3                  :USRFM2:USRFMI, :SRTRE2:SRTREI, :ENDRE2:ENDREI,
     4                  :SRTBL2:SRTBLI, :ENDBL2:ENDBLI, :RECFM2:RECFMI,
     5                  :RECLG2:RECLGI, :BLKLG2:BLKLGI,
     6                  :CREDA2:CREDAI, :CATDA2:CATDAI,
     7                  :ACSDA2:ACSDAI, :ACTIV2,        :CRENA2:CRENAI,
     8                  :CREAC2:CREACI, :CRENO2:CRENOI, :CREJO2:CREJOI,
     9                  :PROTE2:PROTEI, :USRW02:USRW0I, :USRW12:USRW1I,
     A                  :USRW22:USRW2I, :USRW32:USRW3I, :USRW42:USRW4I,
     B                  :USRW52:USRW5I, :USRW62:USRW6I, :USRW72:USRW7I,
     C                  :USRW82:USRW8I, :USRW92:USRW9I, :SYSW02:SYSW0I,
     D                  :SYSW12:SYSW1I, :SYSW22:SYSW2I, :SYSW32:SYSW3I,
     E                  :SYSW42:SYSW4I, :SYSW52:SYSW5I, :SYSW62:SYSW6I,
     F                  :SYSW72:SYSW7I, :SYSW82:SYSW8I, :SYSW92:SYSW9I,
     G                  :COMMT2:COMMTI, :MEDI2:MEDII
 
                  ENDIF
 
*...              Check if VOLUMES_ and FXV_ entries already exist and
*                 insert if not
 
                  CMD = 'STMT6'
                  EXEC SQL OPEN C6 USING :VS2, :VI2
                  EXEC SQL FETCH C6 INTO :VOLID
 
                  IF (SQLERD(3).EQ.0) THEN
                     CMD = 'STMT7'
                     EXEC SQL OPEN C7
                     EXEC SQL FETCH C7 INTO :VOLID
                     VOLID = VOLID + 1
 
                     CMD = 'STMT11'
                     EXEC SQL EXECUTE S11 USING :VOLID, :VS2, :VI2,
     1                                          :PRE2:PREI, :DEN2:DENI
                  ENDIF
 
                  CMD = 'STMT14'
                  EXEC SQL OPEN C14 USING :FILEID, :FLSQN2, :VOLID
                  EXEC SQL FETCH C14 INTO :FILEID
 
                  IF (SQLERD(3).EQ.0) THEN
                     CMD = 'STMT12'
                     EXEC SQL EXECUTE S12 USING :FILEID, :FLSQN2,
     1                                          :VOLID, :VLSQN2:VLSQNI
                  ENDIF
 
               ENDIF
 
            ENDIF
 
         ENDIF
 
   50 CONTINUE
 
      EXEC SQL COMMIT WORK
 
      RETURN
 
 1100 CONTINUE
      WRITE (6,1110) CMD, SQLWN1, SQLWN2, SQLWN3, SQLWN4, SQLWN5,
     2               SQLWN6, SQLWN7
      IRETC = -9998
      EXEC SQL WHENEVER SQLWARNING CONTINUE
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1200 CONTINUE
      WRITE (6,1210) CMD, SQLCDE, (SQLEMC(J), J=1,70)
      IRETC = SQLCDE
      EXEC SQL WHENEVER SQLERROR CONTINUE
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1300 CONTINUE
      WRITE (6,1310) DBASE,EXPER
      IRETC = -9997
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1400 CONTINUE
      WRITE (6,1410) J
      IRETC = -9996
      ITOTAL = J
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1500 CONTINUE
      WRITE (6,1510) J
      IRETC = -9995
      ITOTAL = J
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1110 FORMAT (/, ' SQL WARNING ON ', A6, '. SQLWN_ INDICATORS: ',
     1        7('"',A1,'"',2X))
 1210 FORMAT (/, ' SQL ERROR ON ', A6, ': ', I5, ' ', 70A1)
 1310 FORMAT (/, ' ERROR: UNKNOWN DATABASE AND EXPERIMENT',A,1X,A)
 1410 FORMAT (/, ' ERROR: GENERIC NAME AND FILE (TUPLE ', I2,
     2           ') DO NOT EXIST')
 1510 FORMAT (/, ' ERROR: generic name and file (TUPLE ', I2,
     2           ') already exist, will not be replaced')
      END
 
#if defined(CERNLIB_SQLDS)
      BLOCKDATA
*     For FODEL
      COMMON/STMT04/LSTMT4,STMT4
      CHARACTER*400 STMT4
      INTEGER*4     LSTMT4/400/
*     For FOGET
      COMMON/STMT02/LSTMT2,STMT2
      CHARACTER*2000 STMT2
      INTEGER*2     LSTMT2/2000/
*     For FOPUT
      COMMON/STMT40/LSTMT40,STMT4X
      CHARACTER*300 STMT4X
      INTEGER*2     LSTMT40/300/
      COMMON/STMT0A/LSTMT10,STMT10
      CHARACTER*1300 STMT10
      INTEGER*2     LSTMT10/1300/
      COMMON/STMT0A/LSTMT13,STMT13
      CHARACTER*1500 STMT13
      INTEGER*2     LSTMT13/1500/
      END
#endif
