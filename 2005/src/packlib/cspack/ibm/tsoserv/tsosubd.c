/*
 * $Id: tsosubd.c,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
 *
 * $Log: tsosubd.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
 * Cspack
 *
 */
#if defined(CERNLIB_IBMMVS)
#include "sys/CERNLIB_machine.h"
#include "_cspack/pilot.h"
/*
     TSOSUBD Version 1.1.4 - 19 Feb 1990
*/
 
#define MVS
#include <manifest.h>
#include <bsdtypes.h>
#include <socket.h>
#include <bsdtime.h>
#include <stdio.h>
#include <in.h>
#include <netdb.h>
#include <time.h>
 
#pragma linkage(CMXLATE,OS)
 
#define JCL_OUTPUT_DSN   "TSOSUB.CNTL"
 
#define FALSE (0)
#define TRUE  (1)
 
#define PORTMIN 5001  /* Portnumbers start at PORTMIN+1 */
#define PORTMAX 5500
 
#define BACKLOG 5
#define MAXHOSTNAME 32
 
#define recv_submit_send   RESUWR
 
#define VERSION "1.0.0  1/1/92"
 
int verbose;
int sport     = PORTMIN;
int jobnumber = -1;
 
main(argc,argv) int argc; char **argv;
{
  int listen_socket, jcl_socket;
  int i;
  int port_number;
  struct linger l;
  struct sockaddr_in sa;
  struct sockaddr_in r_sa;
  int r_sa_lng;
  char *inet_ntoa();
  char remote_host[150];
  struct sockaddr isa;
  struct hostent *hp;
  struct servent *sp;
  char localhost[MAXHOSTNAME+1];
  int wake_up_interval;
  int num_fds, nfound;
  unsigned long readfds;
  struct timeval timeout;
  time_t lt;
 
  int i_reuseaddr;
 
 
  port_number = 5001;
  verbose = FALSE;
  wake_up_interval = 0;
 
  for(i = 1; i < argc; ++i) {
 
    if(argv[i][0] != '-') continue;
 
    switch(argv[i][1])
    {
      case 'w':
      case 'W':
          if(++i == argc) continue;
          wake_up_interval = atoi(argv[i]);
          break;
      case 'p':
      case 'P':
          if(++i == argc) continue;
          port_number = atoi(argv[i]);
          break;
      case 'v':
      case 'V':
          verbose = TRUE;
          break;
    }  /* switch (argv.....  */
 } /* for(argn....  */
 
 /*  Get a socket */
  if ((listen_socket = socket(AF_INET,SOCK_STREAM,0)) < 0) {
    tcperror("socket");
    exit(1);
  }
 
  if (verbose) {
    printf("listen_socket = %i\n",listen_socket);
  }
 
 /* I want to reuse address   */
  i_reuseaddr = 1;
  if (
    setsockopt(
    listen_socket, SOL_SOCKET, SO_REUSEADDR,
      (char *) &i_reuseaddr,sizeof(i_reuseaddr)) < 0 ) {
    tcperror("setsocketopt SO_REUSEADDR");
  }
 
 /*  Get the local host information  */
  (void)gethostname(localhost,MAXHOSTNAME);
  if((hp = gethostbyname(localhost)) == NULL) {
    fprintf(stderr,"Cannot get local host info?\n");
    exit(1);
  }
 
 /*  Set the things for the bind call */
  sa.sin_addr.s_addr=INADDR_ANY;
  sa.sin_family = hp->h_addrtype;
  sa.sin_port=port_number;
 
 
 /* bind the socket */
  if(bind(listen_socket,&sa,sizeof sa,0) < 0) {
    tcperror("bind");
    exit(1);
  }
 
  listen(listen_socket,BACKLOG);
 
 /* print out a startup message */
  printf("TSOSVR Version %s   GSI / Darmatadt\n",VERSION);
  time(&lt);
  printf("Server started at %s\n",asctime(localtime(&lt)));
 
 /* Loop forever */
  for (;;) {
 
    if (wake_up_interval != 0) {
       readfds |= (1 << listen_socket);
       timeout.tv_sec=wake_up_interval;
       nfound = select(listen_socket+1,&readfds,0,0,&timeout);
       if (nfound==0) {
          continue;
          }
       }  /* wake_up_interval  */
   /* Just wait for someone to connect to us */
    i = sizeof isa;
    if ((jcl_socket = accept(listen_socket,&isa,&i)) < 0) {
      tcperror("accept");
      exit(1);
    }
 
    r_sa_lng = sizeof r_sa;
    if (getpeername(jcl_socket,&r_sa,&r_sa_lng) != 0) {
      tcperror("accept");
      }
    strcpy(remote_host,inet_ntoa(r_sa.sin_addr.s_addr));
    time(&lt);
    strcat(remote_host," on ");
    strcat(remote_host,asctime(localtime(&lt)));
    remote_host[strlen(remote_host)-1]='\0';
    printf("\nConnection from %s.\n",remote_host);
    fflush(stdout);
 
   /* read and submit JCL */
    if (read_submit(jcl_socket)) {
 
    /* readjcl will return 1 if special "stop-string" is received */
      if ( close(listen_socket,2) < 0) {
        tcperror("close listen_socket");
      }
      if ( close(jcl_socket,2) < 0) {
        tcperror("close jcl_socket");
      }
      printf("Shutting down.\n");
      fflush(stdout);
      fflush(stderr);
      exit(0);
      }
 
   /* all done close the jcl_socket, and go wait for the next connect */
    if ( close(jcl_socket,2) < 0) {
      tcperror("close jcl_socket");
    }
 
    if (verbose) {
      printf("jcl_socket is closed\n");
    }
 
    fflush(stdout);
    fflush(stderr);
  }
}
 
 
int
read_submit(sock)
int sock;
{
  char rmachine[80];
  char ruser[80];
  char rtty[80];
  char mvs_userid[80];
  char mvs_passwd[80];
  char tso_cmd[80];
  char cmvs_sport[80];
  int  mvs_sport;
 
  extern char asciitoebcdic[];
  extern char ebcdictoascii[];
 
 
  char cmd[80];
  char command[80];
  char jobname[80];
  char servreply[80];
 
  int i_open = 0;
 
  register char *c;
 
  FILE *jcl_output;
 
  int k;
  int j=0;
  int i;
 
  int ilines = 7;
  char *cntl??(??) = {
    "//TSO      EXEC LGN001 \n",
    "//SYSIN    DD DUMMY            \n",
    "//SYSPRINT DD SYSOUT=T         \n",
    "//SYSTERM  DD SYSOUT=T         \n",
    "//FT05F001 DD DUMMY            \n",
    "//FT06F001 DD SYSOUT=T         \n",
    "//SYSTSPRT DD SYSOUT=T         \n",
    "//SYSTSIN  DD *                \n"};
 
  if (verbose) {
    printf ("read_submit entered\n");
    fflush(stdout);
  }
 
  /* read strings from socket */
  if ( getstr(sock,rmachine,sizeof(rmachine),"rmachine") <= 0) {
    sprintf(servreply,"Error: Problem in getting rmachine");
    goto bad;
  }
 
  if ( getstr(sock,ruser,sizeof(ruser),"ruser") <= 0) {
    sprintf(servreply,"Error: Problem in getting ruser");
    goto bad;
  }
 
  if ( getstr(sock,rtty,sizeof(rtty),"rtty") <= 0) {
    sprintf(servreply,"Error: Problem in getting rtty");
    goto bad;
  }
 
  if ( getstr(sock,mvs_userid,sizeof(mvs_userid),"mvs_userid") <= 0) {
    sprintf(servreply,"Error: Problem in getting mvs_userid");
    goto bad;
  }
 
  if ( getstr(sock,mvs_passwd,sizeof(mvs_passwd),"mvs_passwd") <= 0) {
    sprintf(servreply,"Error: Problem in getting mvs_passwd");
    goto bad;
  }
 
  if ( getstr(sock,tso_cmd,sizeof(tso_cmd),"tso_cmd") <= 0) {
    sprintf(servreply,"Error: Problem in getting tso_cmd");
    goto bad;
  }
 
  if ( getstr(sock,cmvs_sport,sizeof(cmvs_sport),"cmvs_sport") <= 0) {
    sprintf(servreply,"Error: Problem in getting cmvs_sport");
    goto bad;
  }
 
 
  CMXLATE(rmachine,asciitoebcdic,strlen(rmachine));
  CMXLATE(ruser,asciitoebcdic,strlen(ruser));
  CMXLATE(rtty,asciitoebcdic,strlen(rtty));
  CMXLATE(mvs_userid,asciitoebcdic,strlen(mvs_userid));
  CMXLATE(mvs_passwd,asciitoebcdic,strlen(mvs_passwd));
  CMXLATE(tso_cmd,asciitoebcdic,strlen(tso_cmd));
  CMXLATE(cmvs_sport,asciitoebcdic,strlen(cmvs_sport));
 
  c = mvs_passwd; while (*c) { if (islower(*c)) *c = toupper(*c); c++; }
  c = mvs_userid; while (*c) { if (islower(*c)) *c = toupper(*c); c++; }
  mvs_sport = atoi(cmvs_sport);
 
  if (verbose) {
    printf ("We have read from socket\n");
    printf ("rmachine   : %s \n",rmachine);
    printf ("ruser      : %s \n",ruser);
    printf ("rtty       : %s \n",rtty);
    printf ("mvs_userid : %sº\n",mvs_userid);
    printf ("tso_cmd    : %s \n",tso_cmd);
    printf ("mvs_sport  : %i \n",mvs_sport);
    fflush(stdout);
  }
 
  /* test of mvs_userid/mvs_passwd */
  if (chpass(mvs_userid,mvs_passwd) <0 ){
    sprintf(servreply,
    "Bad user and/or password for user %s.",mvs_userid)                 ;
     goto bad;
  }
 
  /* shutting down */
  if ((!strncmp(mvs_userid,"RZ",2) || !strncmp(mvs_userid,"RS",2))
    &&  !strncmp(tso_cmd,"tsosubend",9) ) {
     sprintf(servreply,"Shutting down TSOSUB.");
     j = 1;
     goto bad;
  }
 
  /* add sport to tso command, if sport == 0  */
  if (mvs_sport == 0 ) {
    if( ++sport >= PORTMAX ) sport = PORTMIN;
    sprintf(command,"%s %d",tso_cmd,sport);
    sprintf(cmvs_sport,"%i",sport);
  }
  else {
    strcpy(command,tso_cmd);
  }
 
  if (verbose) {
    printf(" command = %s\n",command);
  }
 
  /* open jcl_output_dsn */
  if ((jcl_output=fopen(JCL_OUTPUT_DSN,
    "w,blksize=3120,lrecl=80,recfm=fb")) == NULL) {
     sprintf(servreply,
     "Error: MVS-Server: Could not open %s (help data-set on MVS)",
     JCL_OUTPUT_DSN);
      goto bad;
  }
  i_open  = 1;
 
  /* add job card                         */
  if( ++jobnumber >= 100 ) jobnumber = 0;
  sprintf(jobname,"%sTS%02i",mvs_userid,jobnumber);
  fprintf(jcl_output,
    "//%s  JOB CLASS=B,MSGCLASS=T,REGION=7000K,\n",jobname);
  fprintf(jcl_output,
    "//     USER=%s,PASSWORD=%s\n",mvs_userid,mvs_passwd);
 
  /* write to jcl_output_dsn */
  for ( i = 0 ; i <= ilines ; i++ ) {
    fprintf(jcl_output,"%s",cntl??(i??));
  }
 
  /* add command  */
  if (!fprintf(jcl_output,"%s\n",command)) {
    sprintf(servreply,
      "Error: MVS-Server: TSO-Command could not be added to CNTL");
    goto bad;
  }
 
  i_open = 0;
  /* close jcl_output_dsn */
  if (( fclose(jcl_output)) != 0 ) {
    sprintf(servreply,
      "Error: MVS-Server: Could not close %s (help data-set on MVS)",
    JCL_OUTPUT_DSN);
    goto bad;
  }
 
  /* submit jcl_output_dsn */
  sprintf(cmd,"submit %s ",JCL_OUTPUT_DSN);
 
  if (system(cmd) != 0) {
    sprintf(servreply,
      "Error: MVS-Server: System call (submit) ended with an error");
    goto bad;
  }
 
  sprintf(servreply,"Job %s submitted on MVS",jobname);
 
bad:
 
  /* close jcl_output_dsn */
  if (i_open) {
    if (( fclose(jcl_output)) != 0 ) {
      fprintf(stderr,
        "Error: MVS-Server: Could not close %s (help data-set on MVS)"  ,
        JCL_OUTPUT_DSN);
    }
  }
 
  if (verbose) {
    printf("servreply =%s\n",servreply);
  }
 
  CMXLATE(cmvs_sport,ebcdictoascii,strlen(cmvs_sport));
  CMXLATE(servreply,ebcdictoascii,strlen(servreply));
 
  /* send client sport,servreply */
  if ( send(sock, cmvs_sport, strlen(cmvs_sport) + 1, 0) < 0) {
    tcperror("Send sport ");
    exit(1);
  }
  if ( send(sock, servreply, strlen(servreply) + 1, 0) < 0) {
    tcperror("Send servreply ");
    exit(1);
  }
 
   return(j);
}
/* This routines are copied from tcpaw to tsosubd.
 * They are : getstr  chpass chpas reply
 */
 
/*
 * read a string from the socket (sock).
 *
 * Returns: Number of bytes in the string
 *          -1  for an error
 */
 
getstr(sock,buf, cnt, errmesg)
int     *sock;
char    *buf;
int     cnt;              /* sizeof() the char array */
char    *errmesg;         /* in case error message required */
{
      char    c;
      int     k = 0;
 
      do {
            if ( read(sock, &c, 1) != 1) {
                   printf("Error or EOF while reading %s from socket.\n",
                   errmesg);
                   return(-1);                 /* error or  EOF */
            }
            *buf++ = c;
            k++;
            if (--cnt == 0) {
                   printf("%s too long from socket.\n", errmesg);
                   return(-1);
            }
      } while (c!= 0);  /* null byte terminates the string */
 
      return(k-1);
 
}
 
int chpass(user, pass)           /* For IBM */
    char *user, *pass;
{
    char *topoint;
    int i;
    topoint = user;
    while (*topoint != '\0'){
      toupper(*topoint);
      topoint++;
    }
    topoint = pass;
    while (*topoint != '\0'){
      toupper(*topoint);
      topoint++;
    }
    if (CHPAS(user,pass)){  /*if pw check no ok (done in assembler) */
      reply("Bad user/password for %s.\n", user);
      return(-3);
    }
    reply("User %s accepted.\n", user);
    return(1);
}
 
int CHPAS(user, pass)           /* For IBMMVS */
    char *user, *pass;
{
    int i;
    char cmd??(80??);
 
    i = sprintf(cmd,"TSOEXEC PWDCHK2 %s,%s",user,pass);
    i = system(cmd);
    /* i = 0  user,pass ok */
    /* i = 4  user,pass not ok */
    return(i);
}
 
reply(s1, s2, s3)
    char *s1, *s2, *s3;
{
    printf(s1, s2, s3);
    fflush(stdout);
}
 
/*
     TSOSUBD Version 1.1.4 - 19 Feb 1990
*/
 
#define MVS
#include <manifest.h>
#include <bsdtypes.h>
#include <socket.h>
#include <bsdtime.h>
#include <stdio.h>
#include <in.h>
#include <netdb.h>
#include <time.h>
 
#pragma linkage(CMXLATE,OS)
 
#define JCL_OUTPUT_DSN   "TSOSUB.CNTL"
 
#define FALSE (0)
#define TRUE  (1)
 
#define PORTMIN 5001  /* Portnumbers start at PORTMIN+1 */
#define PORTMAX 5500
 
#define BACKLOG 5
#define MAXHOSTNAME 32
 
#define recv_submit_send   RESUWR
 
#define VERSION "1.0.0  1/1/92"
 
int verbose;
int sport     = PORTMIN;
int jobnumber = -1;
 
main(argc,argv) int argc; char **argv;
{
  int listen_socket, jcl_socket;
  int i;
  int port_number;
  struct linger l;
  struct sockaddr_in sa;
  struct sockaddr_in r_sa;
  int r_sa_lng;
  char *inet_ntoa();
  char remote_host[150];
  struct sockaddr isa;
  struct hostent *hp;
  struct servent *sp;
  char localhost[MAXHOSTNAME+1];
  int wake_up_interval;
  int num_fds, nfound;
  unsigned long readfds;
  struct timeval timeout;
  time_t lt;
 
  int i_reuseaddr;
 
 
  port_number = 5001;
  verbose = FALSE;
  wake_up_interval = 0;
 
  for(i = 1; i < argc; ++i) {
 
    if(argv[i][0] != '-') continue;
 
    switch(argv[i][1])
    {
      case 'w':
      case 'W':
          if(++i == argc) continue;
          wake_up_interval = atoi(argv[i]);
          break;
      case 'p':
      case 'P':
          if(++i == argc) continue;
          port_number = atoi(argv[i]);
          break;
      case 'v':
      case 'V':
          verbose = TRUE;
          break;
    }  /* switch (argv.....  */
 } /* for(argn....  */
 
 /*  Get a socket */
  if ((listen_socket = socket(AF_INET,SOCK_STREAM,0)) < 0) {
    tcperror("socket");
    exit(1);
  }
 
  if (verbose) {
    printf("listen_socket = %i\n",listen_socket);
  }
 
 /* I want to reuse address   */
  i_reuseaddr = 1;
  if (
    setsockopt(
    listen_socket, SOL_SOCKET, SO_REUSEADDR,
      (char *) &i_reuseaddr,sizeof(i_reuseaddr)) < 0 ) {
    tcperror("setsocketopt SO_REUSEADDR");
  }
 
 /*  Get the local host information  */
  (void)gethostname(localhost,MAXHOSTNAME);
  if((hp = gethostbyname(localhost)) == NULL) {
    fprintf(stderr,"Cannot get local host info?\n");
    exit(1);
  }
 
 /*  Set the things for the bind call */
  sa.sin_addr.s_addr=INADDR_ANY;
  sa.sin_family = hp->h_addrtype;
  sa.sin_port=port_number;
 
 
 /* bind the socket */
  if(bind(listen_socket,&sa,sizeof sa,0) < 0) {
    tcperror("bind");
    exit(1);
  }
 
  listen(listen_socket,BACKLOG);
 
 /* print out a startup message */
  printf("TSOSVR Version %s   GSI / Darmatadt\n",VERSION);
  time(&lt);
  printf("Server started at %s\n",asctime(localtime(&lt)));
 
 /* Loop forever */
  for (;;) {
 
    if (wake_up_interval != 0) {
       readfds |= (1 << listen_socket);
       timeout.tv_sec=wake_up_interval;
       nfound = select(listen_socket+1,&readfds,0,0,&timeout);
       if (nfound==0) {
          continue;
          }
       }  /* wake_up_interval  */
   /* Just wait for someone to connect to us */
    i = sizeof isa;
    if ((jcl_socket = accept(listen_socket,&isa,&i)) < 0) {
      tcperror("accept");
      exit(1);
    }
 
    r_sa_lng = sizeof r_sa;
    if (getpeername(jcl_socket,&r_sa,&r_sa_lng) != 0) {
      tcperror("accept");
      }
    strcpy(remote_host,inet_ntoa(r_sa.sin_addr.s_addr));
    time(&lt);
    strcat(remote_host," on ");
    strcat(remote_host,asctime(localtime(&lt)));
    remote_host[strlen(remote_host)-1]='\0';
    printf("\nConnection from %s.\n",remote_host);
    fflush(stdout);
 
   /* read and submit JCL */
    if (read_submit(jcl_socket)) {
 
    /* readjcl will return 1 if special "stop-string" is received */
      if ( close(listen_socket,2) < 0) {
        tcperror("close listen_socket");
      }
      if ( close(jcl_socket,2) < 0) {
        tcperror("close jcl_socket");
      }
      printf("Shutting down.\n");
      fflush(stdout);
      fflush(stderr);
      exit(0);
      }
 
   /* all done close the jcl_socket, and go wait for the next connect */
    if ( close(jcl_socket,2) < 0) {
      tcperror("close jcl_socket");
    }
 
    if (verbose) {
      printf("jcl_socket is closed\n");
    }
 
    fflush(stdout);
    fflush(stderr);
  }
}
 
 
int
read_submit(sock)
int sock;
{
  char rmachine[80];
  char ruser[80];
  char rtty[80];
  char mvs_userid[80];
  char mvs_passwd[80];
  char tso_cmd[80];
  char cmvs_sport[80];
  int  mvs_sport;
 
  extern char asciitoebcdic[];
  extern char ebcdictoascii[];
 
 
  char cmd[80];
  char command[80];
  char jobname[80];
  char servreply[80];
 
  int i_open = 0;
 
  register char *c;
 
  FILE *jcl_output;
 
  int k;
  int j=0;
  int i;
 
  int ilines = 7;
  char *cntl??(??) = {
    "//TSO      EXEC LGN001 \n",
    "//SYSIN    DD DUMMY            \n",
    "//SYSPRINT DD SYSOUT=T         \n",
    "//SYSTERM  DD SYSOUT=T         \n",
    "//FT05F001 DD DUMMY            \n",
    "//FT06F001 DD SYSOUT=T         \n",
    "//SYSTSPRT DD SYSOUT=T         \n",
    "//SYSTSIN  DD *                \n"};
 
  if (verbose) {
    printf ("read_submit entered\n");
    fflush(stdout);
  }
 
  /* read strings from socket */
  if ( getstr(sock,rmachine,sizeof(rmachine),"rmachine") <= 0) {
    sprintf(servreply,"Error: Problem in getting rmachine");
    goto bad;
  }
 
  if ( getstr(sock,ruser,sizeof(ruser),"ruser") <= 0) {
    sprintf(servreply,"Error: Problem in getting ruser");
    goto bad;
  }
 
  if ( getstr(sock,rtty,sizeof(rtty),"rtty") <= 0) {
    sprintf(servreply,"Error: Problem in getting rtty");
    goto bad;
  }
 
  if ( getstr(sock,mvs_userid,sizeof(mvs_userid),"mvs_userid") <= 0) {
    sprintf(servreply,"Error: Problem in getting mvs_userid");
    goto bad;
  }
 
  if ( getstr(sock,mvs_passwd,sizeof(mvs_passwd),"mvs_passwd") <= 0) {
    sprintf(servreply,"Error: Problem in getting mvs_passwd");
    goto bad;
  }
 
  if ( getstr(sock,tso_cmd,sizeof(tso_cmd),"tso_cmd") <= 0) {
    sprintf(servreply,"Error: Problem in getting tso_cmd");
    goto bad;
  }
 
  if ( getstr(sock,cmvs_sport,sizeof(cmvs_sport),"cmvs_sport") <= 0) {
    sprintf(servreply,"Error: Problem in getting cmvs_sport");
    goto bad;
  }
 
 
  CMXLATE(rmachine,asciitoebcdic,strlen(rmachine));
  CMXLATE(ruser,asciitoebcdic,strlen(ruser));
  CMXLATE(rtty,asciitoebcdic,strlen(rtty));
  CMXLATE(mvs_userid,asciitoebcdic,strlen(mvs_userid));
  CMXLATE(mvs_passwd,asciitoebcdic,strlen(mvs_passwd));
  CMXLATE(tso_cmd,asciitoebcdic,strlen(tso_cmd));
  CMXLATE(cmvs_sport,asciitoebcdic,strlen(cmvs_sport));
 
  c = mvs_passwd; while (*c) { if (islower(*c)) *c = toupper(*c); c++; }
  c = mvs_userid; while (*c) { if (islower(*c)) *c = toupper(*c); c++; }
  mvs_sport = atoi(cmvs_sport);
 
  if (verbose) {
    printf ("We have read from socket\n");
    printf ("rmachine   : %s \n",rmachine);
    printf ("ruser      : %s \n",ruser);
    printf ("rtty       : %s \n",rtty);
    printf ("mvs_userid : %sº\n",mvs_userid);
    printf ("tso_cmd    : %s \n",tso_cmd);
    printf ("mvs_sport  : %i \n",mvs_sport);
    fflush(stdout);
  }
 
  /* test of mvs_userid/mvs_passwd */
  if (chpass(mvs_userid,mvs_passwd) <0 ){
    sprintf(servreply,
    "Bad user and/or password for user %s.",mvs_userid)                 ;
     goto bad;
  }
 
  /* shutting down */
  if ((!strncmp(mvs_userid,"RZ",2) || !strncmp(mvs_userid,"RS",2))
    &&  !strncmp(tso_cmd,"tsosubend",9) ) {
     sprintf(servreply,"Shutting down TSOSUB.");
     j = 1;
     goto bad;
  }
 
  /* add sport to tso command, if sport == 0  */
  if (mvs_sport == 0 ) {
    if( ++sport >= PORTMAX ) sport = PORTMIN;
    sprintf(command,"%s %d",tso_cmd,sport);
    sprintf(cmvs_sport,"%i",sport);
  }
  else {
    strcpy(command,tso_cmd);
  }
 
  if (verbose) {
    printf(" command = %s\n",command);
  }
 
  /* open jcl_output_dsn */
  if ((jcl_output=fopen(JCL_OUTPUT_DSN,
    "w,blksize=3120,lrecl=80,recfm=fb")) == NULL) {
     sprintf(servreply,
     "Error: MVS-Server: Could not open %s (help data-set on MVS)",
     JCL_OUTPUT_DSN);
      goto bad;
  }
  i_open  = 1;
 
  /* add job card                         */
  if( ++jobnumber >= 100 ) jobnumber = 0;
  sprintf(jobname,"%sTS%02i",mvs_userid,jobnumber);
  fprintf(jcl_output,
    "//%s  JOB CLASS=B,MSGCLASS=T,REGION=7000K,\n",jobname);
  fprintf(jcl_output,
    "//     USER=%s,PASSWORD=%s\n",mvs_userid,mvs_passwd);
 
  /* write to jcl_output_dsn */
  for ( i = 0 ; i <= ilines ; i++ ) {
    fprintf(jcl_output,"%s",cntl??(i??));
  }
 
  /* add command  */
  if (!fprintf(jcl_output,"%s\n",command)) {
    sprintf(servreply,
      "Error: MVS-Server: TSO-Command could not be added to CNTL");
    goto bad;
  }
 
  i_open = 0;
  /* close jcl_output_dsn */
  if (( fclose(jcl_output)) != 0 ) {
    sprintf(servreply,
      "Error: MVS-Server: Could not close %s (help data-set on MVS)",
      JCL_OUTPUT_DSN);
    goto bad;
  }
 
  /* submit jcl_output_dsn */
  sprintf(cmd,"submit %s ",JCL_OUTPUT_DSN);
 
  if (system(cmd) != 0) {
    sprintf(servreply,
      "Error: MVS-Server: System call (submit) ended with an error");
    goto bad;
  }
 
  sprintf(servreply,"Job %s submitted on MVS",jobname);
 
bad:
 
  /* close jcl_output_dsn */
  if (i_open) {
    if (( fclose(jcl_output)) != 0 ) {
      fprintf(stderr,
        "Error: MVS-Server: Could not close %s (help data-set on MVS)"  ,
        JCL_OUTPUT_DSN);
    }
  }
 
  if (verbose) {
    printf("servreply =%s\n",servreply);
  }
 
  CMXLATE(cmvs_sport,ebcdictoascii,strlen(cmvs_sport));
  CMXLATE(servreply,ebcdictoascii,strlen(servreply));
 
  /* send client sport,servreply */
  if ( send(sock, cmvs_sport, strlen(cmvs_sport) + 1, 0) < 0) {
    tcperror("Send sport ");
    exit(1);
  }
  if ( send(sock, servreply, strlen(servreply) + 1, 0) < 0) {
    tcperror("Send servreply ");
    exit(1);
  }
 
   return(j);
}
/* This routines are copied from tcpaw to tsosubd.
 * They are : getstr  chpass chpas reply
 */
 
/*
 * read a string from the socket (sock).
 *
 * Returns: Number of bytes in the string
 *          -1  for an error
 */
 
getstr(sock,buf, cnt, errmesg)
int     *sock;
char    *buf;
int     cnt;              /* sizeof() the char array */
char    *errmesg;         /* in case error message required */
{
      char    c;
      int     k = 0;
 
      do {
            if ( read(sock, &c, 1) != 1) {
                   printf("Error or EOF while reading %s from socket.\n",
                   errmesg);
                   return(-1);                 /* error or  EOF */
            }
            *buf++ = c;
            k++;
            if (--cnt == 0) {
                   printf("%s too long from socket.\n", errmesg);
                   return(-1);
            }
      } while (c!= 0);  /* null byte terminates the string */
 
      return(k-1);
 
}
 
int chpass(user, pass)           /* For IBM */
    char *user, *pass;
{
    char *topoint;
    int i;
    topoint = user;
    while (*topoint != '\0'){
      toupper(*topoint);
      topoint++;
    }
    topoint = pass;
    while (*topoint != '\0'){
      toupper(*topoint);
      topoint++;
    }
    if (CHPAS(user,pass)){  /*if pw check no ok (done in assembler) */
      reply("Bad user/password for %s.\n", user);
      return(-3);
    }
    reply("User %s accepted.\n", user);
    return(1);
}
 
int CHPAS(user, pass)           /* For IBMMVS */
    char *user, *pass;
{
    int i;
    char cmd??(80??);
 
    i = sprintf(cmd,"TSOEXEC PWDCHK2 %s,%s",user,pass);
    i = system(cmd);
    /* i = 0  user,pass ok */
    /* i = 4  user,pass not ok */
    return(i);
}
 
reply(s1, s2, s3)
    char *s1, *s2, *s3;
{
    printf(s1, s2, s3);
    fflush(stdout);
}
 
#endif
