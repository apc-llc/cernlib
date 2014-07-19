*
* $Id: cretab.F,v 1.1.1.1 1996/03/07 15:17:51 mclareni Exp $
*
* $Log: cretab.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:51  mclareni
* Fatmen
*
*
#include "sys/CERNLIB_machine.h"
#include "_fatmen/pilot.h"
#if defined(CERNLIB_ORACLE)
 
INSERT INTO Fatmen VALUES ('CERN', 'throng')
/
 
REM ***   SPECIFIC TABLES FOR throng
 
CREATE TABLE GNames_throng ( GName CHAR(240) NOT NULL,
                             GN# NUMBER NOT NULL)
/
CREATE TABLE Files_throng ( File# NUMBER NOT NULL,
                            GN# NUMBER NOT NULL,
                            Copylevel NUMBER(2) NOT NULL,
                            Location NUMBER NOT NULL,
                            Hostname CHAR(8) NOT NULL,
                            Fullname CHAR(240) NOT NULL,
                            Hosttype CHAR(16),
                            Opersys CHAR(12),
                            Fileformat CHAR(4) NOT NULL,
                            Userformat CHAR(4),
                            Startrec# NUMBER,
                            Endrec# NUMBER,
                            Startblk# NUMBER,
                            Endblk# NUMBER,
                            Recformat CHAR(4),
                            Reclength NUMBER,
                            Blklength NUMBER,
                            Creation DATE,
                            Catalogation DATE,
                            Lastaccess DATE,
                            Active CHAR(1) NOT NULL,
                            Creatorname CHAR(8),
                            Creatoraccount CHAR(8),
                            Creatornode CHAR(8),
                            Creatorjob CHAR(8),
                            Protection NUMBER(2),
                            Userword0 NUMBER,
                            Userword1 NUMBER,
                            Userword2 NUMBER,
                            Userword3 NUMBER,
                            Userword4 NUMBER,
                            Userword5 NUMBER,
                            Userword6 NUMBER,
                            Userword7 NUMBER,
                            Userword8 NUMBER,
                            Userword9 NUMBER,
                            Sysword0 NUMBER,
                            Sysword1 NUMBER,
                            Sysword2 NUMBER,
                            Sysword3 NUMBER,
                            Sysword4 NUMBER,
                            Sysword5 NUMBER,
                            Sysword6 NUMBER,
                            Sysword7 NUMBER,
                            Sysword8 NUMBER,
                            Sysword9 NUMBER,
                            Comments CHAR(80),
                            Mediatype CHAR(1))
/
CREATE TABLE FXV_throng ( File# NUMBER NOT NULL,
                          Fileseq# NUMBER NOT NULL,
                          Vol# NUMBER NOT NULL,
                          Volseq# NUMBER)
/
CREATE TABLE Volumes_throng ( Vol# NUMBER NOT NULL,
                              VSN CHAR(6) NOT NULL,
                              VID CHAR(6) NOT NULL,
                              VIDprefix NUMBER,
                              Density NUMBER)
/
 
GRANT SELECT ON Gnames_throng TO PUBLIC
/
GRANT SELECT ON Files_throng TO PUBLIC
/
GRANT SELECT ON FXV_throng TO PUBLIC
/
GRANT SELECT ON Volumes_throng TO PUBLIC
/
 
#endif
