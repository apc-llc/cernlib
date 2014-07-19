/*
 * $Id: telnetg.c,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
 *
 * $Log: telnetg.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
 * Cspack
 *
 */
#include "sys/CERNLIB_machine.h"
#include "_cspack/pilot.h"
/*
 * User telnet program with CERN Graphics hooks: B.Segal and O.Couet *
  (Version of: 09 Aug 1990)
 *
 *** Modified  by O. Couet/CERN-CN September 1990 tekint replace by imtek
 *** Modified  by O. Couet/CERN-CN September 1990 VMS support in the "G" part
 *** Modified  by B. Segal/CERN-DD August    1990 to add VMS support
 *** Modified  by B. Segal/CERN-DD January   1990 to add SYS5 support
 *** Modified  by O. Couet/CERN-DD May 1989  (IM)  to be compatible with 3270g
 *** Modified  by B. Segal/CERN-DD February  1989 (IM)  for O. Couet's "imint"
 *** Modified  by B. Segal/CERN-DD January   1986 (TEK) for R. Brun's "tekint"
 *** Corrected by B. Segal/CERN-DD November  1984 (BS) ***
 */
 
#define NOTEKIM   /* FOR TESTING ONLY */
 
* For Apollo's prior to SR10.4
#if defined(CERNLIB_APOSR103)
#ifdef apollo
#include "/sys/ins/base.ins.c"
#endif apollo
#endif
 
#ifdef CRAY
#  define USG
#endif CRAY
 
#ifdef linux
#  define USG
#  define LINUX
#endif /* LINUX */
#ifdef hpux    /* Define hpux for HP Unix, 300 or 800 series machines */
#  define USG
#endif /* hpux */
 
#ifdef IRIS50  /* Define IRIS50 for SGI IRIX 5.0 or higher         */
#  define IRIS35
#  define CTRL_quoted
#endif
 
#ifdef IRIS35  /* Define IRIS35 for SGI IRIS with kernel-based TCP */
#  define USG  /* Needs special compile flags: */
#endif IRIS35  /*  cc  telnet.c  -DIRIS35  -I/usr/include/bsd  -lbsd   */
 
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
 
#ifdef VMS
#undef SIGTSTP
#include <iodef.h>
#include <ssdef.h>
#include <climsgdef.h>
#include <descrip.h>
#include <tt2def.h>
#endif VMS
 
#ifdef USG
#ifdef LINUX
#define CEOF EOF
# include <termio.h>
#else
# include <sys/termio.h>
#endif /* LINUX */
/* #include <time.h>             */
# define  bcopy(src, dest, len)    memcpy(dest, src, len)
#endif USG
 
#ifndef hpux
#  include <sys/time.h>  /* may not be needed nowadays...  */
#endif !hpux
 
#ifdef __convexc__
#  define CONVEX
#  define CTRL_quoted
#endif /* __convexc__ */
 
/* Don't include <arpa/inet.h>, 4.2's lies about inet_addr */
extern char *inet_ntoa();
unsigned long inet_addr();
 
#if defined(CERNLIB_DECS)||defined(CERNLIB_APOLLO)||defined(CERNLIB_SUN)||defined(CERNLIB_NEXT)
#undef CTRL
#endif
 
#ifndef CTRL
#  define      CTRL(c)         ('c' & 0x1f)
#endif CTRL
 
#define TELOPTS
#include <arpa/telnet.h>
 
#define strip(x)        ((x)&0177)
 
char    ttyobuf[BUFSIZ], *tfrontp = ttyobuf, *tbackp = ttyobuf;
char    netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;
 
int     dbgt  = 0;         /* 1=>trace+call imtek; -1=> trace only. (BS) */
 
/* Note: imtek expects one argument of type char !!!                     */
 
#ifdef apollo              /*   (TEK/IM).. */
std_$call void   imtek();
std_$call void   imint();
#else
#ifdef CRAY
#define imtek IMTEK
#define imint IMINT
#else
#ifndef VMS
#define imtek imtek_
#define imint imint_
#endif !VMS
#endif CRAY
extern  int      imtek();
extern  int      imint();
#endif apollo
 
int     tek = 0, im = 0, im1 = 0, im2 = 0;
int     flb = 1;                /* flush flag. (add by O.Couet) */
int     imcount = 0;            /* count of IMCHR. (add by O.Couet) */
char    tekarg;
#define IMLEN    80
char    imbuf[IMLEN+2];
char   *imptr =  imbuf;
char   *imlim = &imbuf[IMLEN];
char   *imbgn = &imbuf[0];      /* add by O.Couet */
#ifdef VMS
struct dsc$descriptor_s imbuf2; /* add by O.Couet (September 14, 1990) */
#endif VMS
int     imout;
 
#define LF       10
#define FF       12
#define CR       13
#define IMCHR    '`'            /* `` Start and finish with IM mode */
#define IMEOR    '@'            /* @ Begin and finish a record in IM mode */
#define IMIN     1
#define ASCIIS   32             /* add by O.Couet */
#define ASCIIE   126            /* add by O.Couet */
#define ESC      27             /* ..(TEK)   */
#define TEKON    29
#define TEKOF    31
 
 
char    hisopts[256];
char    myopts[256];
 
char    doopt[] = { IAC, DO, '%', 'c', 0 };
char    dont[] = { IAC, DONT, '%', 'c', 0 };
char    will[] = { IAC, WILL, '%', 'c', 0 };
char    wont[] = { IAC, WONT, '%', 'c', 0 };
 
int     connected;
int     net;
int     showoptions = 0;
int     options;
int     debug = 0;
int     crmod = 0;
char    *prompt;
#ifdef LINUX
char    escape = (']' & 0x1f);
#else
#ifdef CTRL_quoted
char    escape = CTRL(']');
#else
char    escape = CTRL(]);
#endif /* CTRL_quoted */
#endif /* LINUX */
 
char    line[200];
int     margc;
char    *margv[20];
 
jmp_buf toplevel;
jmp_buf peerdied;
 
#ifdef VMS
extern  int noshare errno;
#else
extern  int errno;
#endif VMS
 
int     tn(), quit(), suspend(), bye(), help();
int     setescape(), status(), toggle(), setoptions();
int     setcrmod(), setdebug();
 
int     caltek4(), caltek5(), caltek6();      /*  TEK)  */
 
#define HELPINDENT (sizeof ("connect"))
 
struct cmd {
        char    *name;          /* command name */
        char    *help;          /* help string */
        int     (*handler)();   /* routine which executes command */
};
 
#ifdef CONVEX
char    openhlp[] =    "connect to a site";
#else
char    openhelp[] =    "connect to a site";
#endif
char    closehelp[] =   "close current connection";
char    quithelp[] =    "exit telnet";
char    zhelp[] =       "suspend telnet";
char    debughelp[] =   "toggle debugging";
char    escapehelp[] =  "set escape character";
char    statushelp[] =  "print status information";
char    helphelp[] =    "print help information";
char    optionshelp[] = "toggle viewing of options processing";
char    crmodhelp[] =   "toggle mapping of received carriage returns";
/*  (TEK)..                                          */
char    help4[] =       "call imtek(-4)";
char    help5[] =       "call imtek(-5)";
char    help6[] =       "call imtek(-6)";
/*..(TEK)                                            */
 
struct cmd cmdtab[] = {
#ifdef CONVEX
        { "open",       openhlp,       tn },
#else
        { "open",       openhelp,       tn },
#endif
        { "close",      closehelp,      bye },
        { "quit",       quithelp,       quit },
#ifdef SIGTSTP
        { "z",          zhelp,          suspend },
#endif SIGTSTP
        { "escape",     escapehelp,     setescape },
        { "status",     statushelp,     status },
        { "options",    optionshelp,    setoptions },
        { "crmod",      crmodhelp,      setcrmod },
        { "debug",      debughelp,      setdebug },
        { "?",          helphelp,       help },
/*  (TEK)..                                          */
        { "4",          help4,          caltek4 },
        { "5",          help5,          caltek5 },
        { "6",          help6,          caltek6 },
/*..(TEK)                                            */
        0
};
 
struct sockaddr_in s_in;
 
void    intr(), deadpeer();
char    *control();
struct  cmd *getcmd();
struct  servent *sp;
 
#ifdef VMS
 
int  sys$qiow(),sys$qio(),sys$assign(),sys$wflor(),sys$cancel(),sys$dassgn();
int  ttin = 3, ntin = 4;    /* Event flag assignment hard-coded for now */
int  rtin, rmask, qmask, stat, ttyfmod, nflag = 0, tflag = 0, qcnt = 0;
int  oldcb[3], newcb[3];
char qtibuf[100], qkar[4];
unsigned short tiosb[4], niosb[4];
static $DESCRIPTOR(device, "TT");
 
/* For QIO buffering... */
 
char netbuf[BUFSIZ], *netp;            /* (BS) */
int  netbufsize = 0;
char kbbuf[BUFSIZ], *kbbufp;
int kbbufcount = 0;
#define NET ntin                        /* kludge for select() as net > 32 */
 
#else  !VMS
 
#define NET s
 
#ifdef USG
struct termio oterm, eterm;
#ifdef CTRL_quoted
char   fakestartc = CTRL('Q');
char   fakestopc = CTRL('S');
#else
char   fakestartc = CTRL(Q);
char   fakestopc = CTRL(S);
#endif
char   eofc = CEOF;
 
#ifdef SIGTSTP
#  ifdef hpux
#    include <bsdtty.h>
#  endif hpux
#endif SIGTSTP
 
#else !USG
 
struct tchars otc, etc;
struct ltchars oltc, eltc;
struct sgttyb ottyb, ettyb;
int    olmode, elmode;
#endif !USG
#endif !VMS
/**************************************************************************/
main(argc, argv)
        int argc;
        char *argv[];
{
        sp = getservbyname("telnet", "tcp");
        if (sp == 0) {
                fprintf(stderr, "telnet: tcp/telnet: unknown service\n");
                exit(1);
        }
 
#ifdef VMS
 stat = sys$assign(&device, &rtin, NULL, NULL);
 if (stat != SS$_NORMAL) { fprintf(stderr, "Problem assigning rtin\n");
    exit(1); }
 stat = sys$qiow(NULL, rtin, IO$_SENSEMODE, tiosb,
    NULL, NULL, &oldcb, 12, NULL, NULL, NULL, NULL);
 if (stat != SS$_NORMAL){
    printf("Problem with sensemode(rtin)\n");
    printf("status = %x iosb0 = %x\n", stat, tiosb[0]);
 }
 newcb[0] = oldcb[0]; newcb[1] = oldcb[1];
 newcb[2] = ( oldcb[2] | TT2$M_PASTHRU );
#else  !VMS
 
#ifdef USG
       ioctl(0, TCGETA, &oterm);
       eterm = oterm;
       eofc = oterm.c_cc[VEOF];
#else !USG
       ioctl(0, TIOCGETP, (char *)&ottyb);
       ioctl(0, TIOCGETC, (char *)&otc);
       ioctl(0, TIOCGLTC, (char *)&oltc);
       ioctl(0, TIOCLGET, (char *)&olmode);
       etc = otc;
       ettyb = ottyb;
       eltc = oltc;
       elmode = olmode;
#endif !USG
#endif !VMS
        setbuf(stdin, 0);
        setbuf(stdout, 0);
 prompt = "telnetg";
        if (argc > 1 && !strcmp(argv[1], "-d"))
                debug = SO_DEBUG, argv++, argc--;
        if (argc != 1) {
                if (setjmp(toplevel) != 0)
                        exit(0);
                tn(argc, argv);
        }
        setjmp(toplevel);
        for (;;)
                command(1);
}
 
char    *hostname;
char    hnamebuf[32];
 
tn(argc, argv)
        int argc;
        char *argv[];
{
        register int c;
        register struct hostent *host;
 
        if (connected) {
                printf("?Already connected to %s\n", hostname);
                return;
        }
        if (argc < 2) {
                strcpy(line, "Connect ");
                printf("(to) ");
                gets(&line[strlen(line)]);
                makeargv();
                argc = margc;
                argv = margv;
        }
        if (argc > 3) {
                printf("usage: %s host-name [port]\n", argv[0]);
                return;
        }
        host = gethostbyname(argv[1]);
        if (host) {
                s_in.sin_family = host->h_addrtype;
                bcopy(host->h_addr, (caddr_t)&s_in.sin_addr, host->h_length);
                hostname = host->h_name;
        } else {
                s_in.sin_family = AF_INET;
                s_in.sin_addr.s_addr = inet_addr(argv[1]);
                if (s_in.sin_addr.s_addr == -1) {
                        printf("%s: unknown host\n", argv[1]);
                        return;
                }
                strcpy(hnamebuf, argv[1]);
                hostname = hnamebuf;
        }
        s_in.sin_port = sp->s_port;
        if (argc == 3) {
                s_in.sin_port = atoi(argv[2]);
/*
                if (s_in.sin_port < 0) {
                        printf("%s: bad port number\n", argv[2]);
                        return;
                }
 */
                s_in.sin_port = htons(s_in.sin_port);
        }
#if defined(LINUX) || defined(CONVEX)
        net = socket(AF_INET, SOCK_STREAM, 0);
#else
        net = socket(AF_INET, SOCK_STREAM, 0, 0);
#endif /* LINUX */
        if (net < 0) {
  perror("telnetg: socket");
                return;
        }
 if (debug && setsockopt(net, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
                perror("setsockopt (SO_DEBUG)");
        signal(SIGINT, intr);
        signal(SIGPIPE, deadpeer);
 
        printf("Trying...\n");
#if defined(LINUX) || defined(CONVEX)
        if (connect(net, (caddr_t)&s_in, sizeof (s_in)) < 0) {
#else
        if (connect(net, (caddr_t)&s_in, sizeof (s_in),0) < 0) {
#endif /* LINUX */
  perror("telnetg: connect");
                signal(SIGINT, SIG_DFL);
                return;
        }
        connected++;
        call(status, "status", 0);
        if (setjmp(peerdied) == 0)
                telnet(net);
        fprintf(stderr, "Connection closed by foreign host.\n");
        exit(1);
}
 
/*
 * Print status about the connection.
 */
/*VARARGS*/
status()
{
        if (connected)
                printf("Connected to %s.\n", hostname);
        else
                printf("No connection.\n");
        printf("Escape character is '%s'.\n", control(escape));
        fflush(stdout);
}
 
makeargv()
{
        register char *cp;
        register char **argp = margv;
 
        margc = 0;
        for (cp = line; *cp;) {
                while (isspace(*cp))
                        cp++;
                if (*cp == '\0')
                        break;
                *argp++ = cp;
                margc += 1;
                while (*cp != '\0' && !isspace(*cp))
                        cp++;
                if (*cp == '\0')
                        break;
                *cp++ = '\0';
        }
        *argp++ = 0;
}
 
#ifdef SIGTSTP
/*VARARGS*/
suspend()
{
       register int save;
       register unsigned long oldmask;
 
       save = mode(0);
       signal(SIGTSTP, SIG_DFL);
       kill(getpid(), SIGTSTP);
 
       /* reget parameters in case they were changed */
#ifdef USG
       ioctl(0, TCGETA, &oterm);
#else !USG
       ioctl(0, TIOCGETP, (char *)&ottyb);
       ioctl(0, TIOCGETC, (char *)&otc);
       ioctl(0, TIOCGLTC, (char *)&oltc);
#endif !USG
       (void) mode(save);
}
#endif SIGTSTP
 
/*VARARGS*/
bye()
{
        register char *op;
 
        (void) mode(0);
        if (connected) {
#ifdef VMS
  cancel_qion();
  sys$dassgn(net);
#else !VMS
                shutdown(net, 2);
                close(net);
#endif VMS
                printf("Connection closed.\n");
                connected = 0;
                /* reset his options */
                for (op = hisopts; op < &hisopts[256]; op++)
                        *op = 0;
        }
}
 
/*VARARGS*/
quit()
{
        call(bye, "bye", 0);
        exit(0);
}
 
/*
 * Help command.
 */
help(argc, argv)
        int argc;
        char *argv[];
{
        register struct cmd *c;
 
        if (argc == 1) {
                printf("Commands may be abbreviated.  Commands are:\n\n");
                for (c = cmdtab; c->name; c++)
                        printf("%-*s\t%s\n", HELPINDENT, c->name, c->help);
                return;
        }
        while (--argc > 0) {
                register char *arg;
                arg = *++argv;
                c = getcmd(arg);
                if (c == (struct cmd *)-1)
                        printf("?Ambiguous help command %s\n", arg);
                else if (c == (struct cmd *)0)
                        printf("?Invalid help command %s\n", arg);
                else
                        printf("%s\n", c->help);
        }
}
 
/*
 * Call routine with argc, argv set from args (terminated by 0).
 * VARARGS2
 */
call(routine, args)
        int (*routine)();
        int args;
{
        register int *argp;
        register int argc;
 
        for (argc = 0, argp = &args; *argp++ != 0; argc++)
                ;
        (*routine)(argc, &args);
}
 
#ifdef VMS
       /* Mode treatment for VMS */
mode(f)
       register int f;
{
       static int prevmode = -2;       /* Impossible previous value */
       int onoff, old;
 
       if(f >= 0) {
               if (prevmode == f)
                       return (f);
               old = prevmode;
               prevmode = f;
       } else {
               /* mode(-1) just resets the tty according to eterm */
               f = old = prevmode;
       }
 
       switch (f) {
 
       case 0:
               onoff = 0;
        cancel_qiot();
        stat = sys$qiow(NULL, rtin, IO$_SETMODE, tiosb,
    NULL, NULL, &oldcb, 12, NULL, NULL, NULL, NULL);
        if (stat != SS$_NORMAL){
    printf("Problem with setmode(rtin)\n");
    printf("status = %x iosb0 = %x\n", stat, tiosb[0]);
        }
        ttyfmod = IO$_READVBLK;
        break;
       case 1:
        cancel_qiot();
        stat = sys$qiow(NULL, rtin, IO$_SETMODE, tiosb,
    NULL, NULL, &newcb, 12, NULL, NULL, NULL, NULL);
        if (stat != SS$_NORMAL){
    printf("Problem with setmode(rtin)\n");
    printf("status = %x iosb0 = %x\n", stat, tiosb[0]);
        }
        ttyfmod = IO$_READVBLK | IO$M_NOECHO;
               break;
       case 2:
        cancel_qiot();
        stat = sys$qiow(NULL, rtin, IO$_SETMODE, tiosb,
    NULL, NULL, &newcb, 12, NULL, NULL, NULL, NULL);
        if (stat != SS$_NORMAL){
    printf("Problem with setmode(rtin)\n");
    printf("status = %x iosb0 = %x\n", stat, tiosb[0]);
        }
        ttyfmod = IO$_READVBLK;
        break;
 
       default:
               return(old);
       }
       return (old);
}
#else !VMS
 
#ifdef USG
       /* Set mode of a System V tty */
 
mode(f)
       register int f;
{
       static int prevmode = -2;       /* Impossible previous value */
       register struct termio *tp = &eterm;
       int onoff, old;
 
       if(f >= 0) {
               if (prevmode == f)
                       return (f);
               old = prevmode;
               prevmode = f;
       } else {
               /* mode(-1) just resets the tty according to eterm */
               f = old = prevmode;
       }
 
       switch (f) {
 
       case 0:
               onoff = 0;
               tp = &oterm;
               break;
 
       case 1:
               eterm.c_iflag &= ~ICRNL;
               eterm.c_oflag &= ~(ONLCR|OPOST);
               eterm.c_lflag &= ~(ECHO|ICANON);
               eterm.c_cc[VMIN] = 1;
               eterm.c_cc[VTIME] = 1;
               onoff = 1;
               break;
       case 2:
               eterm.c_iflag |= ICRNL;
               eterm.c_oflag |= ONLCR|OPOST;
               eterm.c_lflag |= ECHO|ISIG|ICANON;
               eterm.c_cc[VEOL] = escape;
               eterm.c_cc[VEOF] = eofc;
               onoff = 1;
               break;
 
       default:
               return(old);
       }
       ioctl(fileno(stdin), TCSETAW, tp);
       ioctl(fileno(stdin), FIONBIO, &onoff);
       /* ioctl(fileno(stdout), FIONBIO, &onoff); */
       return (old);
}
 
#else !USG
       /* Set mode of a 4.xBSD tty */
 
mode(f)
       register int f;
{
       static int prevmode = -2;       /* Impossible previous value */
       int onoff, old;
       register struct tchars *tc;
       register struct ltchars *ltc;
       struct sgttyb sb;
       static struct   tchars notc =   { -1, -1, -1, -1, -1, -1 };
       static struct   ltchars noltc = { -1, -1, -1, -1, -1, -1 };
 
 
       if(f >= 0) {
               if (prevmode == f)
                       return (f);
               old = prevmode;
               prevmode = f;
       } else {
               /* mode(-1) just resets the tty according to etc &c */
               f = old = prevmode;
       }
       switch (f) {
 
       case 0:
               onoff = 0;
               tc = &otc;
               ltc = &oltc;
               sb = ottyb;
               break;
 
       case 1:
               sb = ettyb;
               sb.sg_flags |= CBREAK;
               sb.sg_flags &= ~(ECHO|CRMOD);
               notc.t_intrc = etc.t_intrc;
               notc.t_quitc = etc.t_quitc;
               notc.t_startc = etc.t_startc;
               notc.t_stopc = etc.t_stopc;
               tc = &notc;
               ltc = &noltc;
               onoff = 1;
               break;
 
       case 2:
               sb = ettyb;
               sb.sg_flags &= ~CBREAK;
               sb.sg_flags |= (ECHO|CRMOD);
               etc.t_brkc = escape;
               eltc.t_suspc = -1;
               eltc.t_dsuspc = -1;
               tc = &etc;
               ltc = &eltc;
               onoff = 1;
               break;
 
       default:
               return(old);
       }
       ioctl(fileno(stdin), TIOCSETN, (char *)&sb);
       ioctl(fileno(stdin), TIOCSLTC, (char *)ltc);
       ioctl(fileno(stdin), TIOCSETC, (char *)tc);
       ioctl(fileno(stdin), FIONBIO, &onoff);
       /* ioctl(fileno(stdout), FIONBIO, &onoff); */
       return (old);
}
#endif !USG
#endif !VMS
 
char    sibuf[BUFSIZ], *sbp;
char    tibuf[BUFSIZ], *tbp;
int     scc, tcc;
 
/*
 * Select from tty and network...
 */
telnet(s)
        int s;
{
        register int c;
        int on = 1;
 int tin = fileno(stdin), tout = fileno(stdout);
 
 
        (void) mode(2);
#ifndef VMS
/****   DON'T DO THIS ON A WIN/TCP SYSTEM !! (TEST ON MULTINET)   ******/
 if (ioctl(s, FIONBIO, &on) < 0) perror("telnetg: ioctl FIONBIO");
#endif VMS
 
        im = 0;                             /* (IM)                    */
        tek = tekarg = 0;                   /* (TEK)..                 */
 
 if (dbgt < 0) printf("Init imtek with arg = %d\n", tekarg);
         /*   (TEK/IM).. */
 if (dbgt <= 0) imtek(tekarg);      /* Initialize imtek (TEK) */
 
        for (;;) {
                int ibits = 0, obits = 0;
 
                if (nfrontp - nbackp)       /* Network output waiting */
   obits |= (1 << NET);
                else
                        ibits |= (1 << tin);
                if (tfrontp - tbackp)       /* Screen output waiting  */
                        obits |= (1 << tout);
                else
   ibits |= (1 << NET);
 
                if (scc < 0 && tcc < 0) break;
#ifdef VMS
  vmselect(16, &ibits, &obits, 0, 0);
#else
                select(16, &ibits, &obits, 0, 0);
#endif VMS
                if (ibits == 0 && obits == 0) {
                        sleep(5);
                        continue;
                }
                /*
                 * Something to read from the network...
                 */
  if (ibits & (1 << NET)) {
#ifdef VMS
   scc = netread(s, sibuf, sizeof (sibuf));
#else
                        scc = read(s, sibuf, sizeof (sibuf));
#endif VMS
                        if (scc < 0 && errno == EWOULDBLOCK)
                                scc = 0;
                        else {
                                if (scc <= 0)
                                        break;
                                sbp = sibuf;
                        }
                }
                /*
                 * Something to read from the tty...
                 */
                if (ibits & (1 << tin)) {
#ifdef VMS
   tcc = ttyread(ttin, tibuf, sizeof (tibuf));
#else
                        tcc = read(tin, tibuf, sizeof (tibuf));
#endif VMS
                        if (tcc < 0 && errno == EWOULDBLOCK)
                                tcc = 0;
                        else {
                                if (tcc <= 0)
                                        break;
                                tbp = tibuf;
                        }
                }
 
                while (tcc > 0) {
                        register int c;
 
                        if ((&netobuf[BUFSIZ] - nfrontp) < 2)
                                break;
                        c = *tbp++ & 0377, tcc--;
                        if (strip(c) == escape) {
#ifdef VMS
    cancel_qiot();
#endif VMS
                                command(0);
                                tcc = 0;
                                break;
                        }
                        if (c == IAC)
                                *nfrontp++ = c;
                        *nfrontp++ = c;
                }
#ifdef VMS
  if ((nfrontp - nbackp) > 0)
#else
  if ((obits & (1 << NET)) && (nfrontp - nbackp) > 0)
#endif VMS
                        netflush(s);             /* Output down network */
 
                if (scc > 0)
                        telrcv();                /* Process network input */
#ifdef VMS
  if ((tfrontp - tbackp) > 0)
#else
                if ((obits & (1 << tout)) && (tfrontp - tbackp) > 0)
#endif VMS
                        ttyflush(tout);          /* Output to screen    */
 
        }
        (void) mode(0);
}
 
/* And here comes "vmselect" and friends "netread", "ttyread"....        */
 
#ifdef VMS
 
/* This VMS version of "select" is just enough for tty and network input */
/* Handling of output bits is a kludge: they are just passed through.    */
/* It is WIN/TCP and Multinet compatible. Sorry for you UCX folks...     */
/* NOTE:  In WIN/TCP select only does network I/O.                       */
/*        In Multinet and UCX there isn't any select at all.             */
 
vmselect(dum, ibits, obits, ebits, time)
 int dum, *ibits, *obits, *ebits;
 struct timeval *time;
{
 int ib = 0, ob = 0;
 register int i, stat;
 
 
    /* start 1st qio on tty input..   */
 if (!tflag) {
    stat = sys$qio(ttin, rtin, ttyfmod, tiosb,
    NULL, NULL, qkar, 1, NULL, NULL, NULL, NULL);
    if (stat != SS$_NORMAL){
    printf("Problem with qio(rtin)\n");
    printf("status = %x iosb0 = %x\n", stat, tiosb[0]);
    } else tflag++;
 }
 
    /* start 1st qio on network input.. */
 if (!nflag) {
    stat = sys$qio(ntin, net, IO$_READVBLK, niosb, NULL, NULL,
    netbuf, BUFSIZ, NULL, NULL, NULL, NULL);
    if (stat != SS$_NORMAL){
    printf("Problem with qio(net)\n");
    printf("status = %x niosb[0] = %x error = %d\n",
     stat, niosb[0], (niosb[0]&0x7fff)>>3);
    } else nflag++;
 }
 if (*ibits == 0) {
    return(0);
 }
 if (time == (struct timeval *)0) { /* NOTE: no timeout implemented */
    qmask = 0;
 
    qmask |= (1<<ttin);    /* deal with tty input any time...  */
    if (*ibits & (1<<NET)) qmask |= (1<<ntin);
 
    /* wait on 'OR' of network  and tty input */
    stat = sys$wflor(ttin, qmask);
    if (stat != SS$_NORMAL)
      { printf("wflor stat = %x\n", stat); return(-1); }
 }
 
    /* read event flags of cluster into rmask */
 stat = sys$readef(ttin, &rmask);
 if ((stat != SS$_WASCLR) && (stat != SS$_WASSET))
    { printf("readef stat = %x\n", stat);  return(-1); }
 
    /* check network input (if appropriate).. */
 if ((*ibits & (1<<NET)) && (rmask & (1<<ntin))) {
        if (niosb[0] != SS$_NORMAL) {
    perror("network input error");
    printf("...TWG error=%d\n", (niosb[0]&0x7fff)>>3);
        }
        if ((netbufsize = niosb[1]) < 0) perror("netread");
        ib |= (1<<NET);    /* network input found..  */
        netp = netbuf;     /* ..and all set up for netread().. */
 }
    /* check tty input (any time)..           */
 if (rmask & (1<<ttin))  { /* deal with tty input any time...  */
        tflag = 0;         /* found some, flag for qio restart */
        if (tiosb[0] == SS$_NORMAL && qcnt >= 0) {
    qtibuf[qcnt++] = qkar[0];
        }  else  qcnt = -1;
 }
 if ((*ibits & 1) && (qcnt)) ib |= 1; /* tty input wanted and ready */
 
 *ibits = ib;
/***    NOTE: NUMBER OF BITS IN "obits" NOT IMPLEMENTED YET    **************/
 if (ib > 1) return(2); else return(ib); /* return no. of bits in ib */
}
 
netread(s, sbuf, sbufsize)
char *sbuf;
{
 int n = netbufsize;  /* netbufsize is set by select */
 
 if (n <= 0)
    goto netex;       /* Error or disconnect */
 if (n <= sbufsize) {
    bcopy(netbuf, sbuf, n);
    netbufsize -= n;
 } else {
    n = -2;
    perror("netread: select buffer oversized");
 }
netex:  if (nflag) {      /* it should be set from select..         */
    nflag = 0;     /* COULD restart qio for efficiency here  */
 }
 return(n);
}
 
ttyread(t, tbuf, tbufsize)
char *tbuf;
{
 int n = qcnt;  /* qcnt is set by select */
 
 if (n <= 0)
    goto ttyex;
 if (n <= tbufsize) {
    bcopy(qtibuf, tbuf, n);
    qcnt -= n;
 } else {
    n = -2;
    perror("netread: select buffer oversized");
 }
ttyex:  if (tflag) {      /* it should be set from select..         */
    tflag = 0;     /* COULD restart qio for efficiency here  */
 }
 return(n);
}
 
cancel_qiot()
{
 
 if (tflag) {
  stat = sys$cancel(rtin);
  if (stat != SS$_NORMAL)
      printf("cancel_qiot status = %x\n", stat);
  tflag = 0;
 }
}
 
cancel_qion()
{
 
 if (nflag) {
  stat = sys$cancel(net);
  if (stat != SS$_NORMAL)
      printf("cancel_qion status = %x\n", stat);
  nflag = 0;
 }
}
#endif VMS
 
command(top)
        int top;
{
        register struct cmd *c;
        int oldmode, wasopen;
 
        oldmode = mode(0);
        if (!top)
                putchar('\n');
        else
                signal(SIGINT, SIG_DFL);
        for (;;) {
                printf("%s> ", prompt);
                if (gets(line) == 0) {
                        if (feof(stdin)) {
                                clearerr(stdin);
                                putchar('\n');
                        }
                        break;
                }
                if (line[0] == 0)
                        break;
                makeargv();
                c = getcmd(margv[0]);
                if (c == (struct cmd *)-1) {
                        printf("?Ambiguous command\n");
                        continue;
                }
                if (c == 0) {
                        printf("?Invalid command\n");
                        continue;
                }
                (*c->handler)(margc, margv);
                if (c->handler != help)
                        break;
        }
        if (!top) {
                if (!connected)
                        longjmp(toplevel, 1);
                (void) mode(oldmode);
        }
}
 
/*
 * Telnet receiver states for fsm
 */
#define TS_DATA         0
#define TS_IAC          1
#define TS_WILL         2
#define TS_WONT         3
#define TS_DO           4
#define TS_DONT         5
 
telrcv()
{
        register int c;
        static int state = TS_DATA;
 
        while (scc > 0) {
                c = *sbp++ & 0377, scc--;
                switch (state) {
 
                case TS_DATA:
                        if (c == IAC) {
                                state = TS_IAC;
                                continue;
                        }
/* (TEK)..   */
                        if (c == TEKON) tek = 1;
                        if (tek) {
                                if (c == TEKOF)   tek = 0;
                                if (c >= ESC  ||  c == FF) {
                                   tekarg = c;
                                } else {
                                   tek = 0;  tekarg =  TEKOF;
                                }
    if(dbgt < 0){
       printf("tek = %d c = %d tekarg = %d\n", tek,c,tekarg);
    }
    if(dbgt <= 0) imtek(tekarg);
                                continue;
                        }
/* ..(TEK)   */
 
/* (IM)..    */
/* im1 and im2 are used to suppress the CR/LF after the 2nd IMCHR */
                        if (im1) { im1=0; continue; }
                        if (im2) { im2=0; continue; }
                        if (im) {
                if (c == IMEOR && flb == 1) {   /* flush the imbuf buffer */
                     flb=0;
                     imptr = imbuf;
                     continue;
                }
                                if (c == IMCHR) {
                    imcount = imcount-1;
                    if (imcount == 0 ) {
                         im = 0; im1++; im2++;
                    }
                                } else {
                     if (imptr > imlim) {
                          *imptr = '\0';
                          imptr = imbgn; /* SAFETY */
                          }
                                         if (c >= ASCIIS && c <= ASCIIE )
                                                  *imptr++ = c;
                                }
                                if (c == IMEOR) {
                     flb=1;
                                         imptr--;
                                         if (imptr == imbgn) continue;
                                         imout = IMLEN - (imlim-imptr);
#ifdef apollo
                                         imint(imbuf,imout);
#else
#ifdef VMS  /* add by O.Couet (September 14, 1990) */
                     imbuf2.dsc$b_dtype   = DSC$K_DTYPE_T;
                     imbuf2.dsc$b_class   = DSC$K_CLASS_S;
                     imbuf2.dsc$w_length  = 82;
                     imbuf2.dsc$a_pointer = imbuf;
      imint(&imbuf2,&imout);
#else
                                         imint(imbuf,&imout);
#endif VMS
#endif apollo
                                         if (imout == 1) { /* Send input... */
                                                  register int i;
                                                  for (i=0; i<IMLEN; i++)
                                                      *nfrontp++ = imbuf[i];
                                         }
                                         imptr = imbuf;
                                }
                                continue;
                        }
                        else if (c == IMCHR) {
                imcount = imcount+1;
                if (imcount == 2 ) {
                     im = 1;
                     imptr = imbuf;
                     continue;
                } else {
                     continue;
                }
                        }
             imcount = 0;
/* ..(IM)    */
 
                        *tfrontp++ = c;
                        /*
                         * This hack is needed since we can't set
                         * CRMOD on output only.  Machines like MULTICS
                         * like to send \r without \n; since we must
                         * turn off CRMOD to get proper input, the mapping
                         * is done here (sigh).
                         */
                        if (c == '\r' && crmod) {
                                *tfrontp++ = '\n';
    if ((&ttyobuf[BUFSIZ] - tfrontp) < 2)
    { printf("\nAAAAGH !\n"); break; }  /* (BS) */
                        }
                        continue;
 
                case TS_IAC:
                        switch (c) {
 
                        case WILL:
                                state = TS_WILL;
                                continue;
 
                        case WONT:
                                state = TS_WONT;
                                continue;
 
                        case DO:
                                state = TS_DO;
                                continue;
 
                        case DONT:
                                state = TS_DONT;
                                continue;
 
                        case DM:
#ifdef USG
                                ioctl(fileno(stdout), TCFLSH, NULL);
#else !USG
#ifdef TIOCFLUSH
                                ioctl(fileno(stdout), TIOCFLUSH, 0);
#endif
#endif !USG
                                break;
 
                        case NOP:
                        case GA:
                                break;
 
                        default:
                                break;
                        }
                        state = TS_DATA;
                        continue;
 
                case TS_WILL:
                        printoption("RCVD", will, c, !hisopts[c]);
                        if (!hisopts[c])
                                willoption(c);
                        state = TS_DATA;
                        continue;
 
                case TS_WONT:
                        printoption("RCVD", wont, c, hisopts[c]);
                        if (hisopts[c])
                                wontoption(c);
                        state = TS_DATA;
                        continue;
 
                case TS_DO:
                        printoption("RCVD", doopt, c, !myopts[c]);
                        if (!myopts[c])
                                dooption(c);
                        state = TS_DATA;
                        continue;
 
                case TS_DONT:
                        printoption("RCVD", dont, c, myopts[c]);
                        if (myopts[c]) {
                                myopts[c] = 0;
                                sprintf(nfrontp, wont, c);
                                nfrontp += sizeof (wont) - 2;
                                printoption("SENT", wont, c);
                        }
                        state = TS_DATA;
                        continue;
                }
        }
}
 
willoption(option)
        int option;
{
        char *fmt;
 
        switch (option) {
 
        case TELOPT_ECHO:
                (void) mode(1);
 
        case TELOPT_SGA:
                hisopts[option] = 1;
                fmt = doopt;
                break;
 
        case TELOPT_TM:
                fmt = dont;
                break;
 
        default:
                fmt = dont;
                break;
        }
        sprintf(nfrontp, fmt, option);
        nfrontp += sizeof (dont) - 2;
        printoption("SENT", fmt, option);
}
 
wontoption(option)
        int option;
{
        char *fmt;
 
        switch (option) {
 
        case TELOPT_ECHO:
                (void) mode(2);
 
        case TELOPT_SGA:
                hisopts[option] = 0;
                fmt = dont;
                break;
 
        default:
                fmt = dont;
        }
        sprintf(nfrontp, fmt, option);
        nfrontp += sizeof (doopt) - 2;
        printoption("SENT", fmt, option);
}
 
dooption(option)
        int option;
{
        char *fmt;
 
        switch (option) {
 
        case TELOPT_TM:
                fmt = wont;
                break;
 
        case TELOPT_ECHO:
                (void) mode(2);
                fmt = will;
                hisopts[option] = 0;
                break;
 
        case TELOPT_SGA:
                fmt = will;
                break;
 
        default:
                fmt = wont;
                break;
        }
        sprintf(nfrontp, fmt, option);
        nfrontp += sizeof (doopt) - 2;
        printoption("SENT", fmt, option);
}
 
/*
 * Set the escape character.
 */
setescape(argc, argv)
        int argc;
        char *argv[];
{
        register char *arg;
        char buf[50];
 
        if (argc > 2)
                arg = argv[1];
        else {
                printf("new escape character: ");
                gets(buf);
                arg = buf;
        }
        if (arg[0] != '\0')
                escape = arg[0];
        printf("Escape character is '%s'.\n", control(escape));
        fflush(stdout);
}
 
/*VARARGS*/
setoptions()
{
 
        showoptions = !showoptions;
        printf("%s show option processing.\n", showoptions ? "Will" : "Wont");
        fflush(stdout);
}
 
/*VARARGS*/
setcrmod()
{
 
        crmod = !crmod;
 printf("%s map carriage return on output.\n",
       crmod ? "Will" : "Wont");
        fflush(stdout);
}
 
/*VARARGS*/
setdebug()
{
 
        debug = !debug;
        printf("%s turn on socket level debugging.\n",
                debug ? "Will" : "Wont");
        fflush(stdout);
 if (debug && net > 0
   && setsockopt(net, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
                perror("setsockopt (SO_DEBUG)");
}
 
/*
 * Construct a control character sequence
 * for a special character.
 */
char *
control(c)
        register int c;
{
        static char buf[3];
 
        if (c == 0177)
                return ("^]");
        if (c >= 040) {
                buf[0] = c;
                buf[1] = 0;
        } else {
                buf[0] = '^';
                buf[1] = '@'+c;
                buf[2] = 0;
        }
        return (buf);
}
 
struct cmd *
getcmd(name)
        register char *name;
{
        register char *p, *q;
        register struct cmd *c, *found;
        register int nmatches, longest;
 
        longest = 0;
        nmatches = 0;
        found = 0;
        for (c = cmdtab; p = c->name; c++) {
                for (q = name; *q == *p++; q++)
                        if (*q == 0)            /* exact match? */
                                return (c);
                if (!*q) {                      /* the name was a prefix */
                        if (q - name > longest) {
                                longest = q - name;
                                nmatches = 1;
                                found = c;
                        } else if (q - name == longest)
                                nmatches++;
                }
        }
        if (nmatches > 1)
                return ((struct cmd *)-1);
        return (found);
}
 
void deadpeer()
{
        (void) mode(0);
        longjmp(peerdied, -1);
}
 
void intr()
{
        (void) mode(0);
        longjmp(toplevel, -1);
}
 
ttyflush(fd)
{
        int n;
 
 if ((n = tfrontp - tbackp) > 0)
 
  n = write(fd, tbackp, n);         /* (BS) */
        if (n < 0)
                return;
        tbackp += n;
        if (tbackp == tfrontp)
                tbackp = tfrontp = ttyobuf;
}
 
netflush(fd)
{
        int n;
 
        if ((n = nfrontp - nbackp) > 0)
 
#ifdef VMS
  n = send (fd, nbackp, n);
#else
                n = write(fd, nbackp, n);
#endif VMS
        if (n < 0) {
                if (errno != ENOBUFS && errno != EWOULDBLOCK) {
                        (void) mode(0);
                        perror(hostname);
                        close(fd);
                        longjmp(peerdied, -1);
                        /*NOTREACHED*/
                }
                n = 0;
        }
        nbackp += n;
        if (nbackp == nfrontp)
                nbackp = nfrontp = netobuf;
}
 
/*VARARGS*/
printoption(direction, fmt, option, what)
        char *direction, *fmt;
        int option, what;
{
        if (!showoptions)
                return;
        printf("%s ", direction);
        if (fmt == doopt)
                fmt = "do";
        else if (fmt == dont)
                fmt = "dont";
        else if (fmt == will)
                fmt = "will";
        else if (fmt == wont)
                fmt = "wont";
        else
                fmt = "???";
        if (option < TELOPT_SUPDUP)
                printf("%s %s", fmt, telopts[option]);
        else
                printf("%s %d", fmt, option);
        if (*direction == '<') {
                printf("\r\n");
                return;
        }
        printf(" (%s)\r\n", what ? "reply" : "don't reply");
}
 
/*        (TEK)..       */
caltek4()
{
        tekarg = -4; imtek(tekarg);
}
 
caltek5()
{
        tekarg = -5; imtek(tekarg);
}
 
caltek6()
{
        tekarg = -6; imtek(tekarg);
}
