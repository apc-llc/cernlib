/*
 * $Id: log.c,v 1.4 2006/09/15 09:35:11 mclareni Exp $
 *
 * $Log: log.c,v $
 * Revision 1.4  2006/09/15 09:35:11  mclareni
 * Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
 *
 * Revision 1.3  2003/09/02 12:47:16  mclareni
 * Mods to replace varargs.h (removed from gcc 3.3) with stdarg.h for Linux gcc
 * 3.3.
 *
 * Revision 1.2  2002/05/16 13:43:38  mclareni
 * The IBM syslog is now the same as most other Unix versions.
 *
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990-1993 by CERN/CN/SW/DC
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] = "@(#)log.c       1.22 7/27/94   CERN CN-SW/DC Frederic \
Hemmer";
#endif /* not lint */
 
/* log.c        - generalized logging routines                          */
 
#include <stdio.h>              /* standard input/output definitions    */
#if !defined(vms)
#include <fcntl.h>              /* file control                         */
#else
#include <stdlib.h>             /* general utility definitions          */
#include <unixio.h>             /* VMS's unix-emulation I/O             */
#include <file.h>               /* VMS's BSD 4.2 open() constants       */
#include <unixlib.h>            /* VMS's UNIX emulation prototypes      */
#include <string.h>             /* string manipulation functions        */
#endif /* vms */
#include <time.h>               /* time related definitions             */
 
#if defined(vms) && defined(DECC)
#define __alpha
#endif
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define atoi    DECC$ATOI
#define chmod   DECC$CHMOD
#define close   DECC$CLOSE
#define ctime   DECC$CTIME
#define getenv  DECC$GETENV
#define getpid  DECC$GETPID
#define open    DECC$OPEN
/* sprintf defined in stdio.h */
#define strcmp  DECC$STRCMP
#define strcpy  DECC$STRCPY
#define strlen  DECC$STRLEN
#define time    DECC$TIME
#define write   DECC$WRITE
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
#if !defined(VM) && !defined(IRIX5) && !defined(LINUX)
#include <varargs.h>            /* variable argument list definitions   */
#else
#include <stdarg.h>             /* variable argument list definitions   */
#include <errno.h>              /* standard error numbers & codes       */
#endif /* VM || IRIX5 || LINUX */
#include <log.h>                /* logging options and definitions      */
 
 
static int loglevel=LOG_NOLOG;  /* logging level                        */
static char logname[64];        /* logging facility name                */
static char logfilename[64]=""; /* log file name                        */
 
#if defined(vms)
syslog() {
        return 0;
}
#endif /* vms */
 
#if !defined(VM)
static int pid;                 /* process identifier                   */
static int logfd ;              /* logging file descriptor              */
#if !defined(SOLARIS) && !defined(linux) && !defined(_AIX) \
  && !defined(IRIX5) && !defined(apollo) && !defined(__DARWIN__)
extern int syslog();
#endif /* !SOLARIS && !IRIX5 && !apollo && !linux && !AIX */
extern char *getenv();
#else  /* VM */
static char *console="CONSOLE";/* VM default console userid            */
static FILE *logfp;             /* logging file pointer                 */
 
static int logpriority=LOG_ALERT;
 
int syslog(int priority, ...)
{
        va_list args;           /* arguments                            */
        register char *p;
        register char *message;
        char    buffer[132];
        char    command[200];
 
        va_start(args, priority);
        /* get syslog() priority */
        if ((p = getenv("SYSLOG_PRIORITY")) != NULL)       {
                if (atoi(p) <= priority)       {
                        return(0);
                }
        }
        else {
                /* default logging to LOG_CRIT */
                if (LOG_CRIT <= priority)       {
                        return(0);
                }
        }
        /* get syslog() VM console user id */
        if ((p = getenv("SYSLOG_CONSOLE")) != NULL)     {
                console = p;
        }
 
        message = va_arg(args, char *);
        (void) vsprintf(buffer, message, args);
        strcpy(command,"EXEC TELL ");
        strcat(command, console);
        strcat(command, " ");
        strcat(command, logname);
        strcat(command, ": ");
        strcat(command,buffer);
        (void) system(command);
        va_end(args);
}
#endif /* VM */
 
 
#if !defined(VM)
#if !defined(IRIX5) && !defined(LINUX)
void     logit();
#else
void     logit(int level, ...);
#endif
#else
int      logit(int level, ...);
#endif /* VM || IRIX5 */
void (*logfunc)()=(void (*)())logit;
 
/*
 * Opening log file.
 * Storing the process pid.
 */
void initlog(name, level, output)
char    *name;                  /* facility name                        */
int     level;                  /* logging level                        */
char    *output;                /* output specifier                     */
{
        register char  *p;
 
        loglevel=level;
 
        /* bypass level if set in environment */
        if ((p = getenv("LOG_PRIORITY")) != NULL)       {
                loglevel=atoi(p);
        }
        /*
         * Opening syslog path.
         */
        strcpy(logname,name);
 
#if defined(sun) || defined(CRAY) || defined(sgi)
        openlog(logname, LOG_PID, LOG_USER);
#endif  /* sun || CRAY || sgi */
 
#if defined(apollo) || defined(hpux) || defined(_AIX)
        openlog(logname, LOG_PID, LOG_USER);
#endif  /* apollo || hpux || _AIX */
 
#if (defined(ultrix) && defined(mips))
        openlog(logname, LOG_PID);
#endif /* ultrix && mips */
 
        /*
         * Opening log file and setting logfunc
         * to either syslog or logit.
         */
        if (!strcmp(output,"syslog"))   {
                logfunc=(void (*)())syslog;
        }
        else {
                logfunc=(void (*)())logit;
                if (strlen(output) == 0)        {
#if !defined(VM)
                        logfd= fileno(stderr) ; /* standard error       */
#else
                        logfp=stderr;
#endif
                }
                else {
                        strcpy(logfilename,output);
                }
        }
}
 
/*
 * logit should be called with the following syntax
 * logit(LOG_LEVEL,format[,value,...]) ;
 */
#if !defined(VM)
#if !defined(IRIX5) && !defined(LINUX)
void logit(va_alist)     va_dcl
#else
void logit(int level, ...)
#endif
#else
int logit(int level, ...)
#endif /* VM */
{
        va_list args ;          /* Variable argument list               */
        char    *format;        /* Format of the log message            */
#if !defined(VM) && !defined(vms)
        time_t  clock;
#else
        unsigned long    clock; /* What time is it ?                    */
#endif /* !VM && !vms */
        char    timestr[64] ;   /* Time in its ASCII format             */
        char    line[BUFSIZ] ;  /* Formatted log message                */
#if !defined(VM)
        int     fd;             /* log file descriptor                  */
#endif
#if !defined(VM) && !defined(IRIX5) && !defined(LINUX)
        int     level;          /* Level of the message                 */
 
        va_start(args);         /* initialize to beginning of list      */
        level = va_arg(args, int);
#else
 
        va_start(args, level);
#endif /* VM || IRIX5 || Linux */
        if (loglevel >= level)  {
                format = va_arg(args, char *);
                (void) time(&clock);
                (void) strcpy(timestr,ctime(&clock)+strlen("Ddd "));
                timestr[strlen(timestr)-1-strlen(" YYYY")]='\0';
 
#if !defined(VM)
                pid = getpid();
#if !defined(vms)
                        (void) sprintf(line,"%s %s[%d]: ",timestr,logname,
                            pid) ;
#else /* vms */
                        (void) sprintf(line,"%s %s[%X]: ",timestr,logname,
                            pid) ;
#endif /* vms   */
                        (void) vsprintf(line+strlen(line),format,args);
#else /* VM */
                        (void) sprintf(line,"%s %s: ",timestr,logname) ;
                        (void) vsprintf(line+strlen(line),format,args);
#endif /* VM */
 
                        if (strlen(logfilename)!=0) {
#if !defined(VM)
                                if ( (fd= open(logfilename,O_CREAT|O_WRONLY|
                                    O_APPEND,0666)) == -1 ) {
                                        syslog(LOG_ERR,"open: %s: %m", 
logfilename);
                                        /* FH we probably should retry */
                                        return;
                                }
                                /*
                         * To be sure that file access is enables
                         * even if root umask is not 0
                         */
                                else
                                        (void) chmod( logfilename, 0666 );
                        }
                        else {
                                if  (strlen(logfilename)==0)
                                        fd= fileno (stderr); /* standard error 
*/
                        }
                        (void) write(fd,line,strlen(line)) ;
                        if (strlen(logfilename)!=0)
                                (void) close(fd);
#else
                        if ( (logfp= fopen(output,"a+")) == NULL ) {
                                perror(output);
                                syslog(LOG_ERR,"fopen: %s: %s", output, 
strerror(errno));
                                return(-1);
                        }
                        (void) fwrite(line,strlen(line),1, logfp) ;
                        if (strlen(logfilename)!=0)
                                (void) fclose(logfp);
#endif /* VM */
                }
                va_end(args);
        }
 
        int getloglv()
        {
                return(loglevel);
        }
