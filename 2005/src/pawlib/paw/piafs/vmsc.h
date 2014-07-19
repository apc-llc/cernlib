/*
 * $Id: vmsc.h,v 1.1 1996/04/02 21:54:55 thakulin Exp $
 *
 * $Log: vmsc.h,v $
 * Revision 1.1  1996/04/02 21:54:55  thakulin
 * Renamed from vmsc.inc.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:26  mclareni
 * Paw
 *
 *
 *
 * vmsc.h
 */
#if defined(CERNLIB_VAX)
/*CMZ :  2.06/01 08/12/94  17.14.10  by  Fons Rademakers*/
/*-- Author :    Julian Bunn   17/10/94*/
# define SIGURG 16
# define O_RDONLY       000
# define O_WRONLY       001
# define O_RDWR         002
# define O_NDELAY       004
# define O_NOWAIT       004
# define O_APPEND       010
# define O_CREAT        01000
# define O_TRUNC        02000
# define O_EXCL         04000
# define LOG_ERR        3
# define LOG_INFO       6
# define LOG_DEBUG      7
# define LOG_PID        1
# define LOG_CONS       2
# define LOG_LOCAL0      (16<<3) /* reserved for local use */
# define LOG_LOCAL1      (17<<3) /* reserved for local use */
# define LOG_LOCAL2      (18<<3) /* reserved for local use */
# define LOG_LOCAL3      (19<<3) /* reserved for local use */
# define LOG_LOCAL4      (20<<3) /* reserved for local use */
# define LOG_LOCAL5      (21<<3) /* reserved for local use */
# define LOG_LOCAL6      (22<<3) /* reserved for local use */
# define LOG_LOCAL7      (23<<3) /* reserved for local use */

# define ioctl socket_ioctl
# define fork vfork

/* pwd.h VMS "Password" file entry structure   */

struct passwd {
 char *pw_account; /* User's account name   */
 short pw_astlm; /* AST queue limit   */
 int pw_b_acc_p; /* Batch access primary days  */
 int pw_b_acc_s; /* Batch access secondary days  */
 short pw_biolm; /* Bufferred I/O count limit  */
 int pw_bytlm; /* Buffer I/O byte limit  */
 char *pw_clitab; /* User defined CLI table  */
 int pw_cputim; /* Maximum CPU time limit (session) */
 char *pw_shell; /* Command language Interpreter  */
 char *pw_defdev; /* Default device name   */
 char *pw_defdir; /* Default Directory name  */
 char *pw_dir; /* Default Directory full spec  */
 char *pw_defpriv; /* Default privileges   */
 int pw_dfwscnt; /* Default working set size  */
 short pw_diolm; /* Direct I/O count limit  */
 int pw_d_acc_p; /* Dialup access primary days  */
 int pw_d_acc_s; /* Dialup access secondary days  */
 char pw_encrypt; /* Primary password encrypt algorithm */
 char pw_encrypt2; /* Secondary password encrypt algorithm */
 short pw_enqlm; /* Lock queue limit   */
 char *pw_expir; /* Account expiration date  */
 short pw_fillm; /* Open file limit   */
 int pw_flags; /* Various login flags   */
 int pw_jtquota; /* Jobwide log. nam. table init. byte q.*/
 char *pw_lastl_i; /* Last interactive login date  */
 char *pw_lastl_n; /* Last non-interactive login date */
 char *pw_lgicmd; /* Default login command file  */
 int pw_l_acc_p; /* Local access primary days  */
 int pw_l_acc_s; /* Local access secondary days  */
 short pw_logfail; /* Login failure count   */
 short pw_maxacctj; /* Max # of bat,int and det jobs */
 short pw_maxdet; /* Detached process limit  */
 short pw_maxjobs; /* Active process limit   */
 int *pw_n_acc_p; /* Network access primary days  */
 int *pw_n_acc_s; /* Network access secondary days */
 char *pw_owner; /* Account owner   */
 int pw_pbytlm; /* Paged buffer I/O byte count limit */
 int pw_pgflquota; /* Paging file quota   */
 int pw_prccnt; /* Subprocess creation limit  */
 char pw_pri;  /* Default priority   */
 int pw_primedays; /* Primary and secondary days  */
 char *pw_priv; /* Privileges hold by user  */
 char *pw_passwd; /* Hashed primary password  */
 char *pw_pwd_date; /* Last primary password change date */
 char pw_pwd_length; /* Minimum password length  */
 char *pw_pwd_life; /* Password lifetime   */
 char *pw_pwd2; /* Hashed secondary password  */
 char *pw_pwd2_date; /* Last secondary password change date */
 char pw_quepri; /* Maximum job queue priority  */
 int pw_r_acc_p; /* Remote interactive primary days */
 int pw_r_acc_s; /* Remote interactive secondary days */
 short pw_salt; /* Random password salt   */
 short pw_shrfillm; /* Shared file limit   */
 short pw_tqcnt; /* Time queue entry limit  */
 int pw_uic;  /* UIC     */
 short pw_uid;  /* UIC member number   */
 short pw_gid;  /* UIC group number   */
 char *pw_name; /* User name    */
 int pw_wsextent; /* Working set extent   */
 int pw_wsquota; /* Working set quota   */
};

struct passwd *getpwent(), *getpwuid(), *getpwnam();

#endif
