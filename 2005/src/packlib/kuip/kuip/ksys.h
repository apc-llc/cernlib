/*
 * $Id: ksys.h,v 1.5 1997/09/02 14:50:01 mclareni Exp $
 *
 * $Log: ksys.h,v $
 * Revision 1.5  1997/09/02 14:50:01  mclareni
 * WINNT corrections
 *
 * Revision 1.4  1997/09/01 13:05:33  gunter
 * VMS 7  has sigmask in signal.h
 *
 * Revision 1.3  1997/03/14 17:16:39  mclareni
 * WNT mods
 *
 * Revision 1.2  1996/09/20 14:54:41  cernlib
 * add define MATCH_RE_COMP for linux
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:33:07  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */
/* ksys.h: system dependent defines */

/* update version if structures have changed */
#define KUIP_VERSION 950303


/* identify system if not possible from preprocessor defines */
#if defined(CERNLIB_AIX370)
#include "khaix370.h"
#endif
#if defined(CERNLIB_APOFTN)
#include "khapoftn.h"
#endif
#if defined(CERNLIB_IBMVM)
#include "khibmvm.h"
#endif
#if defined(CERNLIB_IBMMVS)
#include "khibmmvs.h"
#endif
#if defined(CERNLIB_NEWLIB)
#include "khnewlib.h"
#endif


#ifdef AIX370
#  define MACHINE_NAME "IBMAIX"
#  define UNIX
#  define F77_EXTERN_INDIRECT
#endif


#if defined(apollo) || defined(__apollo)
#  define MACHINE_NAME "APOLLO"
#  define APOLLO
#  define UNIX
#  include <apollo/base.h>
#  include <apollo/error.h>
#  include <apollo/ios.h>
#  include <apollo/pad.h>
#  include <apollo/pgm.h>
#  include <apollo/streams.h>
#  ifdef APOLLO_FTN             /* using /com/ftn instead of /bin/f77 */
#    define F77_CHAR_LEN_IND
#    define F77_CHAR_LEN_TYPE short
#    define F77_EXTERN_LOWERCASE
#  endif
#  define F77_EXTERN_INDIRECT
#  define F77_COMMON(name) name __attribute((__section(name)))
#  ifndef EXTERN
#    define EXTERN              /* essential for accessing COMMON blocks */
#  endif
#  define FATAL_SIGFPE
#  define HAVE_MEMMOVE
#  define HAVE_STRCASECMP
#  define NO_DIRENT_H
#  define TERMIO_BSD            /* for getline we must compile under bsd4.3 */
#endif


#if defined(__convexc__)
#  define CONVEX
#  define MACHINE_NAME "CONVEX"
#  define UNIX
#  define F77_BLOCK(name,NAME) _##name##_
#  define F77_ARG_CONSTANT
#  ifndef EXTERN
#    define EXTERN              /* essential for accessing COMMON blocks */
#  endif
#  define FATAL_SIGFPE
#  define HAVE_MEMMOVE
#  define HAVE_STRCASECMP
#  define HAVE_SELECT
#  define HAVE_VFORK
#  define MATCH_RE_COMP         /* use re_comp/re_exec */
#  define TERMIO_MAP_NL         /* need to map NL to NL-CR on output */
#  define USE_EDIT_SERVER
#endif


#ifdef CRAY
#  define MACHINE_NAME "CRAY"
#  define UNIX
#  include <fortran.h>
#  define F77_EXTERN_UPPERCASE
#  ifndef EXTERN
#    define EXTERN              /* essential for accessing COMMON blocks */
#  endif
#  define HAVE_STRCASECMP
#  define NO_EDIT_SERVER
#endif


#if defined(__hpux)
#  define MACHINE_NAME "HPUX"
#  define HPUX
#  define UNIX
#  ifndef _HPUX_SOURCE
#    define _HPUX_SOURCE
#  endif
#  define FATAL_SIGFPE          /* needs f77 +T and ON REAL UNDERFLOW IGNORE */
#  define HAVE_MEMMOVE
#  define HAVE_STRCASECMP
#  define HAVE_STRRSTR
#  define HAVE_SELECT
#  define SELECT_CAST(fds) (int*)fds
#endif


#ifdef _IBMR2
#  define IBMRT
#  define MACHINE_NAME "IBMRT"
#  define UNIX
#  define HAVE_MEMMOVE
#  define HAVE_STRCASECMP
#  include <sys/select.h>
#  define HAVE_SELECT
#  define BROKEN_F77_IO
#endif


#ifdef IBMVM
#  define ARG_STYLE_CMS
#  define MACHINE_NAME "IBM"
#  define OS_NAME "VM"
#  ifndef IBM370
#    define IBM370
#  endif
#  define KmTimeStamp TimeStamp
   typedef char TimeStamp[24];
   extern int get_stamp(char*,TimeStamp*);
#  define same_stamp(stamp1,stamp2) (strcmp(*(stamp1),*(stamp2)) == 0)
#endif

#ifdef IBMMVS
#  define MACHINE_NAME "IBMMVS"
#  define OS_NAME "MVS"
#  ifndef IBM370
#    define IBM370
#  endif
#endif

#ifdef IBM370
#  define F77_ARG_CONSTANT
#  define F77_CHAR_LEN_IND
                              /* each parameter has a length argument ! */
#  define F77_XXXX_ARG_LEN(x) F77_CHAR_ARG_LEN(x)
#  define F77_EXTERN_INDIRECT
#  define F77_EXTERN_UPPERCASE
#  define NO_FCNTL_H
#  define NO_UNISTD_H
#  define SIGNAL_V7
#  define ALLOC_MIN_BYTES 128
#  define STUPID_MALLOC
#endif


#ifdef linux
#  define LINUX
#  define MACHINE_NAME "LINUX"
#  define UNIX
#  define HAVE_MEMMOVE
#  define HAVE_STRCASECMP
#  define HAVE_SELECT
#  define MATCH_RE_COMP		/* use re_comp/re_exec */
#endif


#ifdef MSDOS
#  define MACHINE_NAME "IBMPC"
#  define OS_NAME "MSDOS"
#  define UNIX
#  define NO_EDIT_SERVER
#  define NO_SOCKET_H
#  define NO_SYS_TIME_H
#  define SIGNAL_V7
#  define SLASHES "\\/"
#  define TERMIO_MAP_NL         /* need to map NL to NL-CR on output */
#endif


#if defined(WIN32) || defined(CERNLIB_WINNT)
#  define WINNT
#  ifdef _ALPHA_
#    define MACHINE_NAME "ALPHA"
#  else
#    define MACHINE_NAME "Intel"
#  endif
#  define OS_NAME "Windows"
#  define UNIX
#  define MSDOS

#  ifdef __STDC__
#    undef __STDC__
#  endif

#  include <io.h>
#  include <process.h>
#  include <direct.h>
#  include <windows.h>
#  define text_mode__()
#  define NO_EDIT_SERVER
/* #  define NO_SOCKET_H */
#  define NO_SYS_TIME_H
#  define NO_UNISTD_H
#  define HAVE_STAT_H
#  define SIGNAL_V7
#  define SLASHES "\\/"
#  define TERMIO_MAP_NL         /* need to map NL to NL-CR on output */
#  define HAVE_MEMMOVE
#  ifdef CERNLIB_MSSTDCALL
#    define F77_EXTERN_UPPERCASE
#  else
#    define F77_EXTERN_LOWERCASE
#  endif
#endif


#ifdef NeXT
#  define MACHINE_NAME "NEXT"
#  define UNIX
#  define getcwd(path,maxlen) getwd(path)
#  define F77_BLOCK(lc,uc)    lc
#  define F77_EXTERN_INDIRECT
                              /* each parameter has a length argument ! */
#  define F77_XXXX_ARG_LEN(x) F77_CHAR_ARG_LEN(x)
#  define F77_XXXX_USE_LEN(x) ,286716 /* don't know if value matters */
#  define HAVE_MEMMOVE
#  define HAVE_VFORK
#  define MATCH_RE_COMP         /* use re_comp/re_exec */
#  define NO_DIRENT_H
#  define NO_UNISTD_H
#  define SIGNAL_BSD
#  define TERMIO_BSD
#  define GETPGRP_BSD           /* BSD getpgrp(pid) vs. POSIX getpgrp(void) */
#endif


#ifdef __osf__
#  define UNIX
#  ifdef __alpha
#    define ALPHA
#    define MACHINE_NAME "ALPHA"
#  endif
#  define HAVE_MEMMOVE
#  define HAVE_STRCASECMP
#  define HAVE_SELECT
#  define BROKEN_F77_IO
#endif


#if defined(__sgi)
#  define MACHINE_NAME "SGI"
#  define SGI
#  define UNIX
#  define HAVE_MEMMOVE
#  define HAVE_STRCASECMP
#  define HAVE_SELECT
#endif


#if defined(sun) || defined(__sun)
#  define SUN
#  define UNIX
#  ifdef CERNLIB_SOLARIS
#    define MACHINE_NAME "SOLARIS"
#    define SOLARIS
#    define HAVE_MEMMOVE
#    define HAVE_SELECT
#    define HAVE_STRCASECMP
#    define HAVE_VFORK
#  else
#    define MACHINE_NAME "SUN"
#    define HAVE_SELECT
#    define HAVE_STRCASECMP
#    define HAVE_VFORK
#    include <vfork.h>
#    define MATCH_RE_COMP       /* use re_comp/re_exec */
#    define GETPGRP_BSD         /* BSD getpgrp(pid) vs. POSIX getpgrp(void) */
#  endif
#endif


#if defined(__ultrix)
#  define MACHINE_NAME "DECS"
#  define ULTRIX
#  define UNIX
#  define HAVE_MEMMOVE
#  define HAVE_STRCASECMP
#  define HAVE_VFORK
#  define NEED_STRDUP
#  define TERMIO_BSD
#  define GETPGRP_BSD           /* BSD getpgrp(pid) vs. POSIX getpgrp(void) */
#  define BROKEN_F77_IO
#endif


#ifdef vms
#  define OS_NAME "VMS"
#  ifdef __ALPHA
#    define ALPHA
#    define MACHINE_NAME "ALPHA"
#  else
#    define MACHINE_NAME "VAX"
#    define ConCat(con,cat) con/**/cat
#  endif
#  include <clidef.h>
#  include <descrip.h>
#  include <lib$routines.h>     /* lib$... prototypes */
#  include <processes.h>
#  include <rmsdef.h>
#  include <dvidef.h>
#  include <iodef.h>
#  include <lckdef.h>
#  include <lnmdef.h>
#  include <psldef.h>
#  include <ssdef.h>
#  include <starlet.h>          /* sys$... prototypes */
#  include <stsdef.h>
#  include <ttdef.h>
#  include <tt2def.h>
#  include <unixio.h>
#  include <unixlib.h>
#  ifndef R_OK                  /* no access() modes in unixio.h on VAX/VMS */
#    define F_OK 0
#    define X_OK 1
#    define W_OK 2
#    define R_OK 4
#  endif
#  define ARG_STYLE_VMS
#  define F77_EXTERN_LOWERCASE
#  define HAVE_MEMMOVE
#  define HAVE_STAT_H
#  define HAVE_VFORK            /* actually have only vfork */
#  define NO_FCNTL_H
#  define NO_UNISTD_H
#  define SIGNAL_BSD
#  if __VMS_VER < 70000000
#    define sigmask(sig) (1L << (sig-1)) /* should be in signal.h */
#  endif
#  define USE_EDIT_SERVER       /* only for TPU/DISPLAY=MOTIF */

#  if defined(VAXC) && !defined(__DECC)
#    define STUPID_MALLOC
#  endif

#  define fix_descriptor(dsc,str,n) \
     do { \
       dsc.dsc$b_dtype   = DSC$K_DTYPE_T; \
       dsc.dsc$b_class   = DSC$K_CLASS_S; \
       dsc.dsc$a_pointer = str; \
       dsc.dsc$w_length  = n; \
     } while( 0 )

#  define var_descriptor(dsc,str) \
     do { \
       dsc.dsc$b_dtype   = DSC$K_DTYPE_T; \
       dsc.dsc$b_class   = DSC$K_CLASS_S; \
       dsc.dsc$a_pointer = str; \
       dsc.dsc$w_length  = strlen( dsc.dsc$a_pointer ); \
     } while( 0 )

#endif /* vms */


#include <assert.h>
#include <ctype.h>
#include <errno.h>
#ifndef  NO_FCNTL_H
#include <fcntl.h>
#endif
#include <math.h>          /* contains strtod() and strtol() on some systems */
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#ifdef DBMALLOC
#include "dbmalloc.h"           /* debug malloc */
extern char* getenv(const char*);
extern double strtod(const char*, char**);
#else
#include <stdlib.h>
#endif
#include <time.h>
#ifndef  NO_UNISTD_H
#include <unistd.h>
#endif


#ifndef HAVE_VFORK
#  define vfork fork
#endif


#ifdef UNIX                     /* also defined for MSDOS and Windows/NT*/
#  ifndef OS_NAME
#    define OS_NAME "UNIX"
#  endif
#  include <sys/types.h>
#  include <sys/stat.h>
#  ifndef NO_SYS_TIME_H
#    include <sys/time.h>       /* struct timeval */
#  endif
#  if !defined(MSDOS) && !defined(CERNLIB_WINNT)
#    include <sys/wait.h>
#    ifdef TERMIO_BSD
#      define TERMIO_MAP_NL     /* need to map NL to NL-CR on output */
#      define HAVE_SELECT
#    endif
#    if !defined(TERMIO_BSD) && !defined(TERMIO_SYSV)
#      define TERMIO_POSIX
#    endif
#    if defined(HAVE_SELECT) && !defined(SELECT_CAST)
#      define SELECT_CAST(fds) fds
#    endif
#    include <pwd.h>
#    ifndef NO_DIRENT_H         /* POSIX opendir() */
#      include <dirent.h>
#    else                       /* BSD opendir() */
#      include <sys/dir.h>      /* plus <sys/types.h> */
#      define dirent   direct   /* struct dirent... */
#      ifndef S_IRUSR
#        define S_IRUSR (S_IREAD)   /* read permission, owner */
#        define S_IWUSR (S_IWRITE)  /* write permission, owner */
#        define S_IXUSR (S_IEXEC)   /* execute/search permission, owner */
#      endif
#    endif
#  endif
#  define HAVE_STAT_H
#  ifndef NO_EDIT_SERVER
#    define USE_EDIT_SERVER
#    ifndef F_LOCK              /* BSD file locking */
#      include <sys/file.h>
#      define lockf(fd,op,offs) flock(fd,op)
#      define F_LOCK  LOCK_EX
#      define F_ULOCK LOCK_UN
#    endif
#  endif
#  ifndef SLASHES
#    define SLASHES "/"
#  endif
#endif


#ifdef SUN
#  ifndef CLOCKS_PER_SEC
#    define CLOCKS_PER_SEC  1000000 /* missing in time.h */
#    define difftime(t1,t0) ((double)(t1-t0))
#    define raise(sig)      kill(getpid(),sig)
#  endif
#endif


#ifndef CLOCKS_PER_SEC
#  define CLOCKS_PER_SEC  100   /* missing in VAX/VMS time.h */
#endif


#ifdef HAVE_STAT_H
#  define KmTimeStamp               struct stat
#  define get_stamp(path,stamp)     stat(path,stamp)
#  define same_stamp(stamp1,stamp2) ((stamp2)->st_mtime == (stamp1)->st_mtime)
#endif

#ifndef KmTimeStamp
#  define KmTimeStamp               int
#  define get_stamp(path,stamp)     (*(stamp) = checksum(path))
#  define same_stamp(stamp1,stamp2) (*(stamp1) == *(stamp2))
#endif


#ifdef MATCH_RE_COMP
   extern char *re_comp();
   extern int   re_exec();
#else
   extern char *regcmp();
   extern char *regex();
#endif



/* command line arguments recognized by KUARGS */
#if !defined(ARG_STYLE_CMS) && !defined(ARG_STYLE_VMS)
#  define ARG_STYLE_UNIX
#endif


#ifndef MACHINE_NAME
#  define MACHINE_NAME "UNKNOWN" /* value returned by $MACHINE */
#endif

#ifndef OS_NAME
#  define OS_NAME "UNKNOWN" /* value returned by $OS */
#endif


/* #define EXTERN must be in one routine to allocate space for globals */
#ifndef EXTERN
#  define EXTERN extern
#endif


/*
 * Preprocessor syntax for token concatenation
 */
#ifndef ConCat
#  define ConCat(con,cat) con##cat
#endif


/*
 * Prototyping for C functions
 */
#define C_PROTO_0(name) \
                  name(void)
#define C_PROTO_1(name,arg1) \
                  name(arg1)
#define C_PROTO_2(name,arg1,arg2) \
                  name(arg1,arg2)
#define C_PROTO_3(name,arg1,arg2,arg3) \
                  name(arg1,arg2,arg3)
#define C_PROTO_4(name,arg1,arg2,arg3,arg4) \
                  name(arg1,arg2,arg3,arg4)
#define C_PROTO_5(name,arg1,arg2,arg3,arg4,arg5) \
                  name(arg1,arg2,arg3,arg4,arg5)
#define C_PROTO_6(name,arg1,arg2,arg3,arg4,arg5,arg6) \
                  name(arg1,arg2,arg3,arg4,arg5,arg6)
#define C_PROTO_7(name,arg1,arg2,arg3,arg4,arg5,arg6,arg7) \
                  name(arg1,arg2,arg3,arg4,arg5,arg6,arg7)
#define C_PROTO_8(name,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) \
                  name(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)
#define C_PROTO_9(name,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) \
                  name(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
#define C_PROTO_13(name,a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3) \
                   name(a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3)

#define C_DECL_1(name,t1,p1) \
                 name(t1 p1)
#define C_DECL_2(name,t1,p1,t2,p2) \
                 name(t1 p1,t2 p2)
#define C_DECL_3(name,t1,p1,t2,p2,t3,p3) \
                 name(t1 p1,t2 p2,t3 p3)
#define C_DECL_4(name,t1,p1,t2,p2,t3,p3,t4,p4) \
                 name(t1 p1,t2 p2,t3 p3,t4 p4)
#define C_DECL_5(name,t1,p1,t2,p2,t3,p3,t4,p4,t5,p5) \
                 name(t1 p1,t2 p2,t3 p3,t4 p4,t5 p5)
#define C_DECL_6(name,t1,p1,t2,p2,t3,p3,t4,p4,t5,p5,t6,p6) \
                 name(t1 p1,t2 p2,t3 p3,t4 p4,t5 p5,t6 p6)
#define C_DECL_7(name,t1,p1,t2,p2,t3,p3,t4,p4,t5,p5,t6,p6,t7,p7) \
                 name(t1 p1,t2 p2,t3 p3,t4 p4,t5 p5,t6 p6,t7 p7)
#define C_DECL_8(name,t1,p1,t2,p2,t3,p3,t4,p4,t5,p5,t6,p6,t7,p7,t8,p8) \
                 name(t1 p1,t2 p2,t3 p3,t4 p4,t5 p5,t6 p6,t7 p7,t8 p8)
#define C_DECL_9(name,t1,p1,t2,p2,t3,p3,t4,p4,t5,p5,t6,p6,t7,p7,t8,p8,t9,p9)\
                 name(t1 p1,t2 p2,t3 p3,t4 p4,t5 p5,t6 p6,t7 p7,t8 p8,t9 p9)
#define C_DECL_13(name,t1,p1,t2,p2,t3,p3,t4,p4,t5,p5,t6,p6,t7,p7,t8,p8,\
t9,p9,t10,p10,t11,p11,t12,p12,t13,p13) \
                 name(t1 p1,t2 p2,t3 p3,t4 p4,t5 p5,t6 p6,t7 p7,t8 p8,\
t9 p9,t10 p10,t11 p11,t12 p12,t13 p13)


typedef  int      IntFunc();
typedef  char*    CharFunc();
typedef  char**  pCharFunc();


