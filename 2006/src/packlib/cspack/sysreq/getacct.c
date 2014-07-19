/*
 * $Id: getacct.c,v 1.2 1997/09/02 08:46:18 mclareni Exp $
 *
 * $Log: getacct.c,v $
 * Revision 1.2  1997/09/02 08:46:18  mclareni
 * WINNT mods, mostly cpp defines
 *
 * Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990-1994 by CERN CN-PDP/CS
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] = "@(#)getacct.c   1.7 7/13/94 CERN CN-PDP/CS F. Hemmer";
#endif /* not lint */
 
/* getacct()              Getting the current account id       */
 
#if !defined(CRAY) && !defined(vms)
#include <stdio.h>
#if !defined(apollo) && !defined(WIN32)
#  include <unistd.h>
#endif /* ! apollo */
#include <string.h>
#include <sys/types.h>
#if defined(vms)
#include <pwd_vms.h>
#else
# ifndef WIN32
#   include <pwd.h>
# endif /* WIN32 */
#endif
 
 
/*
 * Check if the environment variable ACCOUNT is set.
 * If YES
 *      Check if it is a valid account id for the user.
 * Else
 *      Get the primary account id of the user.
 * Endif
 *
 * The look-up policy is the same as for passwd.
 */
char * getacct()
{
        extern char     * getenv() ;
        extern char  * ypgetacct() ;
        static char buffer[BUFSIZ] ;            /* Line buffer                 
         */
        char             * account ;            /* Pointer to the account env 
variable  */
        struct passwd        * pwd ;            /* Pointer to the password 
entry        */
        FILE                  * fp ;            /* Pointer to /etc/account 
file         */
 
        /*
         * Getting environmental variable.
         */
        account= getenv("ACCOUNT") ;
 
        /*
         * Getting password entry.
         */
        if ( ( pwd= getpwuid(getuid())) == NULL ) {
                return NULL ;
        }
 
        /*
         * If possible /etc/account is parsed until the record
         * we are looking for is found, '+' is encountered as a user login name
         * ( indicating that we have to look in YP) or the end of file is 
reached.
         */
        if ( (fp= fopen("/etc/account","r")) == NULL ) {
#if defined(NIS)
                return ypgetacct(pwd,account,buffer) ;
#else
                return NULL ;
#endif  /* NIS */
        }
 
        while(fgets(buffer,80,fp) != NULL ) {
                char      * cp ;
 
                if ( (cp= strtok(buffer,":")) == NULL ) {
                        return NULL ;
                }
 
#if defined(NIS)
                if ( *cp == '+' ) {
                        fclose(fp) ;
                        return ypgetacct(pwd,account,buffer) ;
                }
#endif  /* NIS  */
 
                if ( strcmp(pwd->pw_name,cp) )
                        continue ;
 
                /*
                 * Getting account id of the current record.
                 */
                if ( (cp= strtok((char *)NULL,":")) == NULL ) {
                        return NULL ;
                }
 
                /*
                 * If the account id was specified in an
                 * environment variable.
                 */
                if ( account != NULL ) {
                        if ( strcmp(account,cp))
                                continue ;
                }
                else    {
                        /*
                         * We are interested in the
                         * master account id.
                         */
                        char * seq ;
 
                        if ( (seq= strtok((char *)NULL,":") ) == NULL ) {
                                return NULL ;
                        }
                        if ( atoi(seq) )
                                continue ;
                }
                (void) fclose(fp) ;
                return cp ;
        }
        (void) fclose(fp) ;
        return NULL ;
}
#endif /* !CRAY && !vms */
 
#if defined(vms)
#include <ssdef.h>      /* System services definitions                  */
#include <uaidef.h>     /* User authorization definitions               */
#include <rmsdef.h>     /* Record Management definitions                */
#include <string.h>     /* String manipulation definitions              */
#include <stdio.h>      /* Standard Input/Output definitions            */
#include <errno.h>      /* Standard error numbers                       */
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define cuserid DECC$CUSERID
#define strlen  DECC$STRLEN
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
extern int sys$getuai(); /* VMS get user accounting information         */
 
static  char    account[9+1];   /* account string buffer                */
static  int     accountl;       /* account string length                */
 
typedef struct {                /* VMS Item desciptor                   */
        short   bufl;
        short   code;
        char    *buf;
        int     *retl;
} Item;
 
typedef struct  {               /* VMS generic descriptor               */
        int     len;
        char    *buf;
} descrip;
 
static struct   {               /* VMS Item list for $GETUAI            */
        Item    items[57];
        int     eol;
} itemlist = {
        {
        9,      UAI$_ACCOUNT,           account,                &accountl
        },
        0
};
 
 
char * getacct()
{
        descrip name_dsc;               /* username's descriptor        */
        char    name[L_cuserid];        /* username buffer              */
        char    *cp;                    /* character pointer            */
        int     rc;                     /* syscall return code          */
 
        cuserid(name);                  /* C env. must be initialized   */
        name_dsc.len = strlen(name);
        name_dsc.buf = name;
        if ((rc = sys$getuai(0, 0, &name_dsc, &itemlist, 0, 0, 0))
                        != SS$_NORMAL)       {
                if (rc == RMS$_RNF)     {
                        vaxc$errno = rc;
                        errno = EVMSERR;
                        cp = NULL;
                }
                if (!(rc & 0x01))       {
                        vaxc$errno = rc;
                        errno = EVMSERR;
                        cp = NULL;
                }
                else    {
                        account[accountl]='\0';
                        cp = account;
                }
        }
        else    {
                account[accountl]='\0';
                cp = account;
        }
        return(cp);
}
#endif /* vms */
 
#if defined(CRAY)
#include <stdio.h>      /* Standard Input/Output definitions            */
 
char    *getacct()
{
        extern int acctid();
        extern char *acid2nam();
        int     aid;                    /* account id                   */
        char    *cp;                    /* character pointer            */
 
        aid = acctid(0, -1);
        if ((aid = acctid(0,-1)) == -1)       {
                cp = NULL;
        }
        else    {
                if ((cp  = acid2nam(aid)) == NULL)     {
                        cp = NULL;
                }
        }
        return(cp);
}
#endif /* CRAY */
 
/*
 * Fortran wrapper
 */
 
/*FH*   to be done      RC = XYACCT()           */
 
